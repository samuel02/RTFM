/*
* RTFM-RT.h
* WIN32 run-time for RTFM-core
* (C) 2014 Per Lindgren, Marcus Lindner
*/


#ifndef RTFM_RT_H_
#define RTFM_RT_H_

typedef void(*ENTRY_FUNC)(int);
void user_reset(int);

void RTFM_lock(int, int);
void RTFM_unlock(int, int);
void RTFM_pend(int,int);

#define SLEEP(x)		Sleep(1000*x)
#endif /* RTFM_RT_H_ */
