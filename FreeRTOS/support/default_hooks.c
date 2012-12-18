
__attribute__((weak))
void vApplicationTickHook(void) {}

__attribute__((weak))
void vApplicationStackOverflowHook(void) {
	for(;;);
}
