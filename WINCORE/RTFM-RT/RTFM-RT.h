/*
 * RTFM-RT.h
 * WIN32 run-time system for RTFM-core
 * (C) 2014 Per Lindgren, Marcus Lindner
 */

#ifndef RTFM_RT_H_
#define RTFM_RT_H_

typedef void(*ENTRY_FUNC)();

void RTFM_lock(int r);
void RTFM_unlock(int r);
void RTFM_pend(int t);

#define SEC(x)		(1000*x)
#endif /* RTFM_RT_H_ */
