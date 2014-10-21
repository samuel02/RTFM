//*****************************************************************************
// RTFM-core.c
// Copyright Per Lindgren 2014
// The "Root" for the compilation of a RTFM-core program
//*****************************************************************************
#include "RTFM-kernel.h"

#ifdef __COMPCERT_RAW__
extern void _vStackTop(void);

void ResetISR(void);
void DefaultISR(void) {
	while (1) ;
}

#pragma section ISR ".isr_vector" "not_used" standard R
#pragma use_section ISR isr_vector

#include "../Application/RTFM_isr_vector.h"
#endif

#include "../Application/autogen.c"

#define NELEMS(x)  (sizeof(x) / sizeof(x[0]))

void RTFM_init_priorities() {
	unsigned int i;
	for (i = 0; i < NELEMS(entry_vi); i++) {
		RTFM_set_priority(entry_vi[i], entry_prio[i]);
		if (entry_vi[i]>= 0) {
			/* do not enable core interrupts (<0) */
			RTFM_enable_irq(entry_vi[i]);
		}
	}
}

#define RT_TIMER
// Entry point for the System (RTFM-kernel)

int main() {
	RTFM_disable();
	RTFM_init_priorities();
#ifdef RT_TIMER
	T_timer_init();
#endif
	user_reset();
	int i = 0;
	i = i + 1; // to break
	RTFM_enable();
	user_idle();
	while (1)
		;
}

