// API to the run-time system functions besides schdeuling 
#ifndef RTFM_RT_H_
#define RTFM_RT_H_

#include <stdint.h>

#define RT_sec                  1000000
#define RT_ms					1000
typedef int64_t RTFM_time;
#define RT_time					RTFM_time

// prototypes
RTFM_time RTFM_get_bl(int id);
void RTFM_set_bl(int id);

#define RT_time_sub(t1, t2)     (t1 - t2)
#define RT_time_add(t1, t2)     (t1 + t2)
#define RT_time_to_float(t)     ( ((float) t)/RT_sec )
#define RT_time_of_float(f)     ((RTFM_time) (f * RT_sec))
#define RT_time_to_us(t)		(t)

// Run-time API, built in functions
#define RT_set_bl()     	    { RTFM_set_bl(RTFM_id); }
#define RT_get_bl()         	RTFM_get_bl(RTFM_id)

#define RT_rand(x)      		( rand() % x )
#define RT_sleep(x) 			( sleep( x ) )
#define RT_usleep(x)        	( usleep( x ) )
#define RT_printf(...) 			{ printf( __VA_ARGS__ ); }

#endif /* RTFM_RT_H_ */
