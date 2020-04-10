extern "C" __attribute__((visibility("default"))) unsigned char mvee_atomic_preop_trampoline(unsigned char a, void* b)
{
	return 0;
}

extern "C" __attribute__((visibility("default"))) void mvee_atomic_postop_trampoline(unsigned char preop_result)
{

}
