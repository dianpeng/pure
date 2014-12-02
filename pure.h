#ifndef PURE_H_
#define PURE_H_
#include <stddef.h>
#include <assert.h>

#define UTEST

/* Fancy is a stupid configuration file format and it supports
 * some simple arithmetic evaluation and it is still a line
 * based configuration file format */

struct pure;
struct pure_result;
struct pure_array;
struct pure_map;
struct pure_value {
    int type;
    union {
        double num;
        void* shared_val;
    } value;
};

#define PURE_NIL 1
#define PURE_NUMBER 2
#define PURE_STRING 3
#define PURE_ARRAY 4
#define PURE_MAP 5
#define PURE_USER_DATA 6

typedef int (*pure_cb)( struct pure* p , 
                        const struct pure_value* par , size_t sz , 
                        struct pure_value* result , 
                        void* );

typedef void (*pure_user_data_clean_cb)( void* udata );
typedef int (*pure_foreach_cb)( const char* name , struct pure_value* val , void* data );

struct pure* pure_create();
void pure_delete( struct pure* );
void pure_reg_func( struct pure* , const char* name , pure_cb cb , void* );
void pure_reg_var( struct pure* , const char* name , struct pure_value* val );
struct pure_result* pure_run_file( struct pure* , const char* file );
struct pure_result* pure_run_str( struct pure* , const char* str , size_t len );
void pure_result_delete( struct pure_result* );

/* pure result */
int pure_get( struct pure_result* result , const char* name , struct pure_value* val );
int pure_foreach( struct pure_result* result , pure_foreach_cb cb ,void* udata );

/* pure value creation */
void pure_value_new_num( struct pure* , double n , struct pure_value* );
void pure_value_new_str( struct pure* , const char* str , int own , struct pure_value* );
struct pure_array* pure_value_new_arr( struct pure* , size_t cap , struct pure_value* );
struct pure_map* pure_value_new_map( struct pure* , size_t cap , struct pure_value* );
#define pure_value_new_nil(p,v) do { (v)->type = PURE_NIL; } while(0)
void pure_value_new_user_data( struct pure* , void* udata , pure_user_data_clean_cb cb , struct pure_value* );

/* pure value ref/unref */
void pure_value_ref( struct pure* , struct pure_value* , const struct pure_value* );
void pure_value_unref( struct pure* , struct pure_value* );

/* pure value getter */
#define pure_value_get_num(val,num) do { *(num) = (val)->num; } while(0)
int pure_value_get_str( const struct pure_value* , const char** );
int pure_value_get_arr( const struct pure_value* , struct pure_array** arr );
int pure_value_get_user_data( const struct pure_value* , void** udata );

/* pure array reference */
struct pure_value* pure_array_index( const struct pure_array* arr , size_t idx );
void pure_array_push( struct pure* p , struct pure_array* arr , const struct pure_value* val );
void pure_array_copy( struct pure* p , struct pure_array* lhs , const struct pure_array* val );

/* pure map reference */


#endif /* PURE_H_ */