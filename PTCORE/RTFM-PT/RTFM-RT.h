// API to the run-time system functions besides scheduling
#ifndef RTFM_RT_H_
#define RTFM_RT_H_

#include <stdint.h>

#define RT_sec                  1000000
#define RT_ms					1000
typedef int64_t RTFM_time;
typedef int RTFM_msg;
#define RT_time					RTFM_time
#define RT_msg					RTFM_msg
#define RTFM_inherit			(-1)

// prototypes
RTFM_time RTFM_get_bl(int id);
void RTFM_set_bl(int id);
bool RTFM_abort(int id);
RTFM_time time_get();

#define RT_time_sub(t1, t2)     (t1 - t2)
#define RT_time_add(t1, t2)     (t1 + t2)
#define RT_time_to_float(t)     ( ((float) t)/RT_sec )
#define RT_time_of_float(f)     ((RTFM_time) (f * RT_sec))
#define RT_time_to_us(t)		(t)

// Run-time API, built in functions
#define RT_set_bl()     	    { RTFM_set_bl(RTFM_id); }
#define RT_get_bl()         	( RTFM_get_bl(RTFM_id) )

#define RT_rand(i)      		( rand() % i )
#define RT_sleep(s) 			( sleep( s ) )
#define RT_usleep(us)        	( usleep( us ) )
#define RT_printf(...) 			{ printf( __VA_ARGS__ ); }
#define RT_getc()				( getc(stdin) )
#define RT_putc(c)				{ putc(c, stdout); }

#define RT_halt(x)				{ if (x) {while (1) { ; }} else return; }
#define RT_abort(t)				{ RTFM_abort(t); }

#endif /* RTFM_RT_H_ */
