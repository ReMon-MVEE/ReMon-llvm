//
// Created by babrath on 4/8/2020.
//

// LLVM headers
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/LineIterator.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TypeSize.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

// C/C++ headers
#include <cstdint>
#include <string>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "ShmSupport"

namespace
{
  llvm::cl::opt<std::string> ShmSupportList("shm_support",
      llvm::cl::desc("List of instructions that access shared memory and need MVEE support"));

  class ShmSupport : public FunctionPass
  {
    public:
      ShmSupport() : FunctionPass(ID)
    {
      initializeShmSupportPass(*PassRegistry::getPassRegistry());
    }

      typedef StringMap<std::set<std::pair<unsigned,unsigned>>> SourceLocationMap;
      static const uint64_t TagMask = 0xFFFF800000000000ULL;
      static char ID;

      StringRef getPassName() const override { return "ShmSupport"; }
      bool doInitialization(Module &M) override;
      bool runOnFunction(Function& M) override;

    private:
      Function* ShmAccessFunc;
      SourceLocationMap ShmAccessLocations;
      bool accessesSharedMemory(const Instruction& I) const;
      void readShmAccessedLocations(const std::string);
      auto getShmInstructions(Function& F);
  };

  bool ShmSupport::accessesSharedMemory(const Instruction& I) const
  {
    const DiagnosticLocation DL(I.getDebugLoc());
    if (DL.isValid())
    {
      const auto Locations = ShmAccessLocations.lookup(DL.getAbsolutePath());
      if (Locations.find(std::pair<unsigned, unsigned>(DL.getLine(), DL.getColumn())) != Locations.end())
        return true;
    }

    return false;
  }

  void ShmSupport::readShmAccessedLocations(const std::string) {
    // Read file, return if failing that
    ErrorOr<std::unique_ptr<MemoryBuffer>> BufferOrErr = MemoryBuffer::getFileOrSTDIN(ShmSupportList);
    if (std::error_code EC = BufferOrErr.getError())
      return;

    // Parse file line by line
    const auto Buffer = std::move(BufferOrErr.get());
    line_iterator Line(*Buffer);
    while (!Line.is_at_end())
    {
      // Parse the line
      llvm::SmallVector<llvm::StringRef, 3> Tokens;
      Line->split(Tokens, ':');
      unsigned ColumnNr;
      unsigned LineNr;
      if (Tokens[1].getAsInteger(0, LineNr))
        return;
      if (Tokens[2].getAsInteger(0, ColumnNr))
        return;

      // Add the location
      auto& F = ShmAccessLocations[Tokens[0]];
      F.emplace(LineNr, ColumnNr);

      // Next
      Line++;
    }
  }

  auto ShmSupport::getShmInstructions(Function& F)
  {
    SmallPtrSet<Instruction *, 32> ShmInstructions;
    for (BasicBlock& BB : F)
    {
      for (Instruction& I : BB)
      {
        if (accessesSharedMemory(I))
        {
          switch (I.getOpcode())
          {
            // Memory operations we wrap
            case Instruction::Load:
            case Instruction::Store:
              {
                ShmInstructions.insert(&I);
                break;
              }
            // Memory operations we might wrap, but disabled for now
            case Instruction::AtomicRMW:
            case Instruction::AtomicCmpXchg:
              // Memory operations we do **not** wrap
            case Instruction::Alloca:
            case Instruction::Fence:
            case Instruction::GetElementPtr:
              break;
            default:
              {
                // Don't handle these instructions
                if (I.isCast())
                  break;

                // All the other instructions: If they Use a LoadInst, wrap it
                for (Value* V : I.operand_values())
                  if (LoadInst *LI = dyn_cast<LoadInst>(V))
                    ShmInstructions.insert(LI);
                break;
              }
          }
        }
      }
    }

    return ShmInstructions;
  }

  bool ShmSupport::doInitialization(Module &M) {
    if (ShmSupportList.empty())
      return false;

    readShmAccessedLocations(ShmSupportList);
    if (ShmAccessLocations.empty())
      return false;

    // mvee_shm_op_ret mvee_shm_op_trampoline(unsigned char id, bool atomic, void* address, unsigned long size, unsigned long value, unsigned long cmp)
    auto& Context = M.getContext();
    StructType* ShmAccessRetTy = StructType::create({Type::getInt64Ty(Context), Type::getInt1Ty(Context)}, "struct.mvee_shm_op_ret");
    FunctionType* ShmAccessTy = FunctionType::get(ShmAccessRetTy,
        {Type::getInt8Ty(Context), Type::getInt1Ty(Context), Type::getInt64PtrTy(Context), Type::getInt64Ty(Context), Type::getInt64Ty(Context), Type::getInt64Ty(Context)}, false);
    ShmAccessFunc = Function::Create(ShmAccessTy, GlobalValue::LinkageTypes::ExternalLinkage, "mvee_shm_op_trampoline", &M);
    return true;
  }

  bool ShmSupport::runOnFunction(Function& F)
  {
    if (ShmAccessLocations.empty())
      return false;

    // Gather all instructions accessing shared memory
    auto ShmInstructions = getShmInstructions(F);
    if (ShmInstructions.empty())
      return false;

    errs().changeColor(raw_ostream::GREEN);
    errs() << "Rewriting " << ShmInstructions.size() << " instructions that access shared memory...\n";
    errs().resetColor();

    auto& Context = F.getContext();
    for (Instruction* ShmInst : ShmInstructions)
    {
      // Gather information on instruction
      const DataLayout &DL = ShmInst->getModule()->getDataLayout();
      Value* Addr = nullptr;
      Value* Cmp = nullptr;
      unsigned ID = -1;
      Type* RetType = Type::getVoidTy(Context);
      uint64_t Size = 0;
      Value* Val = nullptr;
      if (LoadInst *LI = dyn_cast<LoadInst>(ShmInst)) {
        Addr = LI->getPointerOperand();
        ID = 0;
        Size = DL.getTypeStoreSize(LI->getType());
        RetType = LI->getType();
      } else if (StoreInst *SI = dyn_cast<StoreInst>(ShmInst)) {
        Addr = SI->getPointerOperand();
        ID = 1;
        Size = DL.getTypeStoreSize(SI->getValueOperand()->getType());
        Val = SI->getValueOperand();
      } else if (AtomicRMWInst *RMW = dyn_cast<AtomicRMWInst>(ShmInst)) {
        Addr = RMW->getPointerOperand();
        ID = 2 + ((RMW->getOperation() - AtomicRMWInst::BinOp::FIRST_BINOP) << 2);
        RetType = RMW->getType();
        Size = DL.getTypeStoreSize(RMW->getValOperand()->getType());
        Val = RMW->getValOperand();
      } else if (AtomicCmpXchgInst *XCHG = dyn_cast<AtomicCmpXchgInst>(ShmInst)) {
        Addr = XCHG->getPointerOperand();
        Cmp = XCHG->getCompareOperand();
        ID = 3;
        RetType = XCHG->getType();
        Size = DL.getTypeStoreSize(XCHG->getCompareOperand()->getType());
        Val = XCHG->getNewValOperand();
      } else if (auto* CI = dyn_cast<CallInst>(ShmInst)) {
        // Inline asm
        // Intrinsics
      }

      IRBuilder<> IRB(ShmInst);

      // If clause
      IntegerType *PtrInt = IRB.getInt64Ty();
      Value *AddrLong = IRB.CreatePointerCast(Addr, PtrInt);
      Value *Tag = IRB.CreateAnd(AddrLong, TagMask);
      Value *TagCheck = IRB.CreateICmpNE(Tag, ConstantInt::getNullValue(PtrInt));
      Instruction *ShmAccessTerm = SplitBlockAndInsertIfThen(TagCheck, ShmInst, false);
      assert(cast<BranchInst>(ShmAccessTerm)->isUnconditional());

      // Arguments:
      Value* Args[6] = {
        ConstantInt::get(Type::getInt8Ty(Context), ID),
        ConstantInt::get(Type::getInt1Ty(Context), ShmInst->isAtomic()),
        Addr,
        ConstantInt::get(Type::getInt64Ty(Context), Size),
        Val ? Val : ConstantInt::get(Type::getInt64Ty(Context), 0),
        Cmp ? Cmp : ConstantInt::get(Type::getInt64Ty(Context), 0)
      };

      // Then clause
      IRB.SetInsertPoint(ShmAccessTerm);
      BasicBlock* ThenBB = ShmAccessTerm->getParent();
      Value* Ret = IRB.CreateCall(ShmAccessFunc, Args);
      if (!ShmInst->use_empty() && RetType->isIntegerTy())
      {
        Ret = IRB.CreateExtractValue(Ret, 0);
        Ret = IRB.CreateIntCast(Ret, RetType, false);
      }

      // Successor
      Instruction *Successor  = ShmInst->getNextNonDebugInstruction();
      IRB.SetInsertPoint(Successor);
      BasicBlock* SuccessorBB = SplitBlock(Successor->getParent(), Successor);
      ThenBB->getTerminator()->setSuccessor(0, SuccessorBB);
      if (!ShmInst->use_empty())
      {
        PHINode* P = IRB.CreatePHI(ShmInst->getType(), 2);
        ShmInst->replaceAllUsesWith(P);
        P->addIncoming(Ret, ThenBB);
        P->addIncoming(ShmInst, ShmInst->getParent());
      }
    }

    return true;
  }

} // namespace

char ShmSupport::ID = 0;
INITIALIZE_PASS(ShmSupport, "ShmSupport", "Rewrite all instructions accessing shared memory to use MVEE support", false, false)

namespace llvm
{
  FunctionPass * createShmSupportPass()
  {
    return new ShmSupport();
  }
} // namespace llvm
