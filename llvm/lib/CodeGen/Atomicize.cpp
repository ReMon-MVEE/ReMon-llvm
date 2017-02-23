//
// Created by stijn on 2/2/17.
//

#include "llvm/IR/PassManager.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/ADT/Statistic.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <iterator>

using namespace llvm;

#define DEBUG_TYPE "Atomicize"
#define ATOMICIZE_DEBUG

STATISTIC(NumAtomicType1, "Number of type 1 instructions wrapped");
STATISTIC(NumAtomicType2, "Number of type 2 instructions wrapped");
STATISTIC(NumAtomicType3, "Number of type 3 instructions wrapped");
STATISTIC(NumAtomicTotal, "Total Number of instructions wrapped");

namespace 
{
	llvm::cl::opt<bool>
	AtomicizeEnabled("atomicize",
		  llvm::cl::desc("Add MVEE hooks to all atomic operations"),
		  llvm::cl::init(0));
	
	
	class Atomicize : public ModulePass
	{
	public:

		Atomicize()
			: ModulePass(ID)
			, PreopType(nullptr)
			, PostopType(nullptr)
			, PreopFunc(nullptr)
			, PostopFunc(nullptr)
			, module(nullptr)
			{
				initializeAtomicizePass(*PassRegistry::getPassRegistry());
			}


		static char ID;
		
		// We add the definitions for the following functions to the Module:
		//   unsigned char mvee_atomic_preop  (unsigned short op_type, void* atomic_word);
		//   void          mvee_atomic_postop (unsigned char preop_result)
		FunctionType* PreopType, *PostopType;
		Function* PreopFunc, *PostopFunc;
		Module* module;
		
		StringRef getPassName() const override { return "Atomicize"; }
		bool runOnModule(Module& M) override;
		void dumpSourceLine(const char* reason, const DILocation* debugLoc);
	};

	struct WrapAtomicsVisitor : public InstVisitor<WrapAtomicsVisitor>
	{
	public:
		WrapAtomicsVisitor(Atomicize* A) : Atomic(A) {}

		void wrapInstInternal(Instruction& I, Value* Ptr, bool modified)
			{
				IRBuilder<> B(&I);

				// possibly insert bitcast to cast the pointer to unsigned long*
				Type* ptrType = TypeBuilder<unsigned long*, false>::get(Atomic->module->getContext());

				if (Ptr->getType() != ptrType)
					Ptr = B.CreateBitCast(Ptr, ptrType);

				// Build preop call
				Value* PreopArgs[2] = {
					// the first argument is the type of atomic instruction.
					// We currently pass type 1 for any instruction that modifies the memory and type 0 otherwise
					ConstantInt::get(Type::getInt16Ty(I.getContext()), modified ? 1 : 0, true),
					// The memory address modified/referenced by the atomic instruction
					Ptr};

				// Build and insert call before the wrapped instruction
				CallInst* PreopCall = B.CreateCall(Atomic->PreopFunc, PreopArgs);

				// We pass the result of the preop call as the sole argument for the postop function
				// And we insert the postop call AFTER the wrapped instruction
				B.SetInsertPoint(I.getNextNode());
				B.CreateCall(Atomic->PostopFunc, {PreopCall});
			}

		void wrapLoadInst(llvm::LoadInst& I)
			{
				wrapInstInternal(I, I.getPointerOperand(), false);
			}

		template <typename InstType> void wrapInst(InstType& I)
			{
				wrapInstInternal(I, I.getPointerOperand(), true);
			}

		void visitAtomicCmpXchgInst(AtomicCmpXchgInst& I)
			{
				Atomic->dumpSourceLine("Wrapping atomic cmpxchg instruction",
									   I.getDebugLoc().get());
			    wrapInst<AtomicCmpXchgInst>(I);
				NumAtomicType1++;
			}

		void visitAtomicRMWInst(AtomicRMWInst& I)
			{
				Atomic->dumpSourceLine("Wrapping atomic instruction",
									   I.getDebugLoc().get());
				wrapInst<AtomicRMWInst>(I);
				NumAtomicType1++;
			}

		void visitLoadInst (LoadInst& I)
			{
				if (I.isAtomic() || I.isVolatile())
				{
					Atomic->dumpSourceLine("Wrapping unprotected load",
										   I.getDebugLoc().get());
					wrapLoadInst(I);
					NumAtomicType3++;
				}
			}

		void visitStoreInst (StoreInst& I)
			{
				if (I.isAtomic() || I.isVolatile())
				{
					Atomic->dumpSourceLine("Wrapping unprotected store",
										   I.getDebugLoc().get());
					wrapInst<StoreInst>(I);
					NumAtomicType3++;
				}
			}

	private:
		Atomicize* Atomic;
	};

	bool Atomicize::runOnModule(Module& M)
	{
		if (!AtomicizeEnabled)
			return false;
		
		module = &M;

		// unsigned char mvee_atomic_preop  (unsigned short operation_type, unsigned long* affected_location)
		// void          mvee_atomic_postop (unsigned char  preop_result)
		PreopType = TypeBuilder<unsigned char(unsigned short, unsigned long*), false>::get(M.getContext());
		PreopFunc = Function::Create(PreopType, GlobalValue::LinkageTypes::ExternalLinkage, "mvee_atomic_preop_trampoline", &M);
		PostopType = TypeBuilder<void(unsigned char), false>::get(M.getContext());
		PostopFunc = Function::Create(PostopType, GlobalValue::LinkageTypes::ExternalLinkage, "mvee_atomic_postop_trampoline", &M);

		errs().changeColor(raw_ostream::GREEN);
		errs() << "Wrapping atomic instructions...\n";
		errs().resetColor();

		WrapAtomicsVisitor V(this);
		V.visit(M);

		NumAtomicTotal = NumAtomicType1 + NumAtomicType2 + NumAtomicType3;		
		errs().changeColor(raw_ostream::YELLOW);
		errs() << "All Done! Found atomics [type1, type2, type3, tot]: ["
			   << NumAtomicType1 << ", "
			   << NumAtomicType2 << ", "
			   << NumAtomicType3 << ", "
			   << NumAtomicTotal << "]\n";
		errs().resetColor();

		return true;
	}
	
// Highly inefficient debug info dumper
// We should keep a cache of ifstreams and line number->file offset map for each ifstream
	void Atomicize::dumpSourceLine(const char* reason, const DILocation* debugLoc)
	{
#ifdef ATOMICIZE_DEBUG
		// Sanity checks
		if (!debugLoc ||
			!debugLoc->getScope() ||
			!debugLoc->getScope()->getDirectory().data() ||
			!debugLoc->getScope()->getFilename().data())
		{
			errs() << reason << " - No debugging information available\n";
			return;
		}

		// Build full filename
		std::stringstream sourceFile;
		sourceFile << debugLoc->getScope()->getDirectory().data() << "/" <<
			debugLoc->getScope()->getFilename().data();

		errs() << reason << " @ " << sourceFile.str() << " [line: " << debugLoc->getLine() << ", column: " << debugLoc->getColumn() << "]\n";

		std::ifstream moduleFile(sourceFile.str());
		std::string sourceLine;

		if (!moduleFile.is_open())
		{
			errs() << "Couldn't open source file.\n";
			return;
		}

		for (unsigned i = 0; i < debugLoc->getLine(); ++i)
			std::getline(moduleFile, sourceLine);

		if (!moduleFile.eof())
		{
			std::replace(sourceLine.begin(), sourceLine.end(), '\t', ' ');

			errs() << sourceLine << "\n";
			errs().indent(debugLoc->getColumn());
			errs() << "^\n";
		}
#endif
	}


}

char Atomicize::ID = 0;
INITIALIZE_PASS(Atomicize, "atomicize",
				"Add MVEE instrumentation to all atomic instructions in the program", false, false)

namespace llvm
{
	ModulePass * createAtomicizePass()
	{
		return new Atomicize();
	}
}


