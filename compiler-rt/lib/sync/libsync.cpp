extern "C" __attribute__((visibility("default"))) unsigned char mvee_atomic_preop_trampoline(unsigned char a, void* b)
{
	return 0;
}

extern "C" __attribute__((visibility("default"))) void mvee_atomic_postop_trampoline(unsigned char preop_result)
{

}

struct mvee_shm_op_ret {
  unsigned long val;
  bool cmp;
};

extern "C" __attribute__((visibility("default"))) mvee_shm_op_ret mvee_shm_op_trampoline(unsigned char id, bool atomic, void* address, unsigned long size, unsigned long value, unsigned long cmp)
{
  return mvee_shm_op_ret();
}
