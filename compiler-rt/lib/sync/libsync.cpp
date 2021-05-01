extern "C" __attribute__((weak)) __attribute__((visibility("default"))) unsigned char mvee_atomic_preop(unsigned char, void*);
extern "C" __attribute__((weak)) __attribute__((visibility("default"))) void mvee_atomic_postop(unsigned char);

extern "C" unsigned char mvee_atomic_preop_trampoline(unsigned char type, void* variable)
{
    if (mvee_atomic_preop)
        return mvee_atomic_preop(type, variable);
    else
        return 0;
}

extern "C" void mvee_atomic_postop_trampoline(unsigned char preop_result)
{
    if (mvee_atomic_postop)
        mvee_atomic_postop(preop_result);
}
