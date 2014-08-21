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
void RTFM_pend(ULONGLONG, int,int);

#define RT_sec                  1000
#define RT_ms					1
typedef ULONGLONG RTFM_time;
#define RT_time					RTFM_time

#define RT_time_to_float(t)     ( ((float) t)/RT_sec )
#endif /* RTFM_RT_H_ */
