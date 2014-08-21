// RTFM-core for RTFM-PT
const char* CORE_FILE_INFO = "Compiled with : RTFM-core options:\ninfile     \t: ../../PTCORE/RTFM-SRC/CEx1.core\noutfile    \t: ../../PTCORE/RTFM-SRC/CEx1.c\ntarget     \t: RTFM_PT\nbackend    \t: GCC\nverbose    \t: false\ndebug      \t: false\ndotout     \t: false\ndotfile    \t: \nldotout    \t: false\nldotfile   \t: \nd_ast      \t: false\n";

// RTFM-Resources
enum resources {RES_NR};
char* res_names[] = {};
int ceilings[RES_NR] = {};
// RTFM-Entry points
enum entry_nr {user_reset_nr, Root_a_q_nr, ENTRY_NR};
int entry_prio[] = {0, 3};
void Root_a_q(int);
ENTRY_FUNC entry_func[] = {user_reset, Root_a_q};
char* entry_names[] = {"user_reset", "Root_a_q"};

// RTFM-Application 
// top level code 

// generating RTFM-core code for Root:Root
// method prototypes for Root:Root
int Root_x (int, char) ;

// class instance parameters for Root:Root

// class instance variables for Root:Root
int Root_i = 5;
int Root_j = 18;

// generating RTFM-core code for A:Root_a
// method prototypes for A:Root_a
int Root_a_m (int) ;
// task Root_a_q();

// class instance parameters for A:Root_a
const int Root_a_a = 1;
const int Root_a_b = 2;
int (* const Root_a_cb) (int, char)  = Root_x;

// class instance variables for A:Root_a
int Root_a_c = 7;
int Root_a_d = 0;

// methods declarations for A:Root_a
int Root_a_m (int Root_a_c) {
	Root_a_d = Root_a_cb (1, 'c') ;
	return Root_a_d;
}

void Root_a_q(int RTFM_id) {

	Root_a_c = Root_a_b;


}
// top level code 



// methods declarations for Root:Root
int Root_x (int Root_r, char Root_c) {
	return Root_r;
}

void user_reset(int RTFM_id) {

	int Root_s = Root_a_m (1) ;


}
// top level code 












