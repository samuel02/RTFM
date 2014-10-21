// RTFM-core for RTFM-RT
const char* CORE_FILE_INFO = "Compiled with : RTFM-core options:\ninfile       : ../../PTCORE/RTFM-SRC/IsrTest3.core\noutfile      : /Users/pln/Documents/LPCXpresso_7.4.0/workspace/RTFM/Application/autogen.c\nasync_err    : false\ntarget       : RTFM_KERNEL\nbackend      : GCC\nverbose      : true\ndebug        : false\ngv_task      : false\ngv_taskf     : \ngv_res       : false\ngv_resf      : \nd_ast        : true\n";

/*
 * Timer Queue Length
 */
#define TQ_LEN 10

/*
 * Queue and Timer
 */
#include "../RTFM-def.h"
#include "../queue.c"


#include <stdbool.h>

enum resources {RES_NR};
int ceilings[] = {};
const char* res_names[] = {};
enum entry_nr {SysTick_Handler_nr = -1, WDT_IRQHandler_nr = 0};
const int entry_vi[] = {SysTick_Handler_nr, WDT_IRQHandler_nr};
int entry_prio[] = {2, 1};
const char* entry_names[] = {"SysTick_Handler", "WWDG_IRQHandler"};

// Isr prio 2 SysTick_Handler
void entry_SysTick_Handler(int RTFM_id); // function prototype for the instance task
volatile rt_time_t bl_SysTick_Handler = 0;
volatile rt_time_t new_bl_SysTick_Handler = 0;
const rt_time_t dl_SysTick_Handler = 100000000;

// Isr prio 1 WDT_IRQHandler
void entry_WDT_IRQHandler(int RTFM_id); // function prototype for the instance task
volatile rt_time_t bl_WDT_IRQHandler = 0;
volatile rt_time_t new_bl_WDT_IRQHandler = 0;
const rt_time_t dl_WDT_IRQHandler = 200000000;

// Reset void entry_user_reset(int RTFM_id); // function prototype for the instance task
volatile rt_time_t bl_user_reset = 0;
volatile rt_time_t new_bl_user_reset = 0;
const rt_time_t dl_user_reset = 2147483647;

// Idle void entry_user_idle(int RTFM_id); // function prototype for the instance task
volatile rt_time_t bl_user_idle = 0;
volatile rt_time_t new_bl_user_idle = 0;
const rt_time_t dl_user_idle = 2147483647;


// Isr prio 2 SysTick_Handler
void SysTick_Handler() {

  rt_time_t ct;
  ct = T_CURR();

  rtfm_lock_t lq = RT_lock(2);

  bl_SysTick_Handler = new_bl_SysTick_Handler;

  while ((ct - tq_h->bl) >= 0) {
    // E_yes
    RTFM_pend(tq_h->id);

    if (tq_deq() == NULL) {
      // D_no
      T_DISABLE();
      RT_unlock(lq);

      return;
    }

    RT_unlock(lq);
    lq = RT_lock(2);
  }

  T_SET(tq_h->bl);
  RT_unlock(lq);
}

volatile int tp = 0;
volatile uint32_t trace[TQ_LEN + 1];

// Isr prio 1 WDT_IRQHandler
void WWDG_IRQHandler() {
  uint32_t ct_temp = T_CURR();
  bl_WDT_IRQHandler = new_bl_WDT_IRQHandler;

  // Debug information
  trace[(tp++)% (TQ_LEN + 1)] = ct_temp; // store current time
}

volatile uint32_t trace2[TQ_LEN+1];

void user_reset() {
  int i = 0;

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 10000000;
  RTFM_async(10000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 20000000;
  RTFM_async(20000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 30000000;
  RTFM_async(30000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 40000000;
  RTFM_async(40000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(50000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(60000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(70000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(80000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(90000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();
  new_bl_WDT_IRQHandler = bl_user_reset + 50000000;
  RTFM_async(100000000, 2147483647, bl_user_reset, WDT_IRQHandler_nr);

  trace2[i++] = T_CURR();

  T_BREAK(0);
}

void user_idle() {
  while (1) {
    T_BREAK(1);
    __WFI();
  }
}
