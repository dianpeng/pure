#ifndef PURE_H_
#define PURE_H_
#include <stddef.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Pure is a small script designed for configuration. It is a _LINE_ based configuration
 * and also it contains some simple operation ability to do some logic . Pure implements
 * a _ONE_ pass parsing and interpret it directly, no code generation. It features a C 
 * syntax + JSON like data structure. It can be used as both complex or simple application
 * configuration */

struct pure;
struct pure_array;
struct pure_map;
struct pure_value {
    int type;
    union {
        double num;
        void* shared_val;
    } value;
};

#define PURE_INVALID 0 /* If a pure_value is this type, then it is not a valid value */
#define PURE_NIL 1
#define PURE_NUMBER 2
#define PURE_STRING 3
#define PURE_ARRAY 4
#define PURE_MAP 5
#define PURE_USER_DATA 6

/*
 * Registered function prototype. The parameter is passing as pure_value array and the
 * parameter size is stored in sz. All the value you get from the parameter is not owned
 * by this caller. So you want to return the parameter into the result, you need to do this:
 * pure_value_ref( result , par[0] ); 
 * This will increase the reference count . In addition, please do not _STORE_ the parameter
 * pointer, if you want to get the value from the parameter, use copy . If execution error 
 * happened, you could set a last error message and return -1 to stop the execution , otherwise
 * return 0 to indicate normal execution */
 
typedef int (*pure_cb)( struct pure* p , 
                        struct pure_value* par , size_t sz , 
                        struct pure_value* result , 
                        void* );

typedef void (*pure_user_data_clean_cb)( void* udata );
typedef int (*pure_foreach_cb)( const char* name , struct pure_value* val , void* data );

/* Create Pure object , after using it, please call pure_delete to release the memory */
struct pure* pure_create();
/* Delete the pure object. After deletion, all the returned object will become invalid */
void pure_delete( struct pure* );
/* Register a function that has name "name" to be called in the script. */
void pure_reg_func( struct pure* , const char* name , pure_cb cb , void* );
/* Register a variable that has name "name" to be retrieved in the script. */
void pure_reg_var( struct pure* , const char* name , struct pure_value* val );
/* Log the last execution error for user defined functions */
void pure_print_error( struct pure* , const char* fmt , ... );

/* The following function will try to run a script resides in the file or a script in string ,
 * The error string is a string record the reason for the error , ec is error code , line 
 * and pos indicate where the error happened */
int pure_run_file( struct pure* , const char* file , const char** error , int* ec , int* line , int* pos );
int pure_run_str( struct pure* , const char* str , const char** error , int* ec , int* line , int* pos );

/* This function is used to retrieve the variable that inside of the file's global scope. The typical
 * usage is put those configuration field in the global scope and then execute the script, later on 
 * use pure_get to retrieve these variables value. */
int pure_get( struct pure* , const char* name , struct pure_value** val );

/* For each allows you to get all the value one by one */
int pure_foreach( struct pure* , pure_foreach_cb cb ,void* udata );

/* These function is used to attach a type of value into pure_value. */
void pure_value_new_num( struct pure* , double n , struct pure_value* );
void pure_value_new_str( struct pure* , const char* str , int own , struct pure_value* );
struct pure_array* pure_value_new_arr( struct pure* , size_t cap , struct pure_value* );
struct pure_map* pure_value_new_map( struct pure* , size_t cap , struct pure_value* );
void pure_value_new_nil( struct pure* , struct pure_value* );
void pure_value_new_user_data( struct pure* , void* udata , pure_user_data_clean_cb cb , struct pure_value* );

/* Reference count management for the pure_value */
void pure_value_ref( struct pure* , struct pure_value* , const struct pure_value* );
void pure_value_unref( struct pure* , struct pure_value* );

/* Deep copy of the value */
void pure_value_copy( struct pure* , struct pure_value* cp );

/* Pure value getter */
static
int pure_value_get_num( const struct pure_value* val , double* num ) {
    if(val->type != PURE_NUMBER)
        return -1;
    *num = val->value.num;
    return 0;
}
int pure_value_get_str( const struct pure_value* , const char** );
int pure_value_get_arr( const struct pure_value* , struct pure_array** arr );
int pure_value_get_map( const struct pure_value* , struct pure_map** map );
int pure_value_get_user_data( const struct pure_value* , void** udata );

/* pure array reference */
size_t pure_array_size( const struct pure_array* arr );
int pure_array_index( const struct pure_array* arr , size_t idx , struct pure_value** val );
void pure_array_push( struct pure* p , struct pure_array* arr , const struct pure_value* val );

/* pure map reference */
int pure_map_index( const struct pure_map* map , const char* key , struct pure_value** val );
int pure_map_insert( struct pure* p , const struct pure_map* map , const char* key , struct pure_value* val );

/* Map iterator. The use pattern is like this:
 * const char* key ;
 * struct pure_value* val;
 * int curosr = pure_map_iter_start(m,&key,&val);
 * while( pure_map_iter_has_next(m,cursor) ) {
 * 		Do some stuff with Key and Value
 *      cursor = pure_map_iter_deref(m,cursor,&key,&val);
 * } */
 
int pure_map_iter_start( const struct pure_map* map , const char** key , struct pure_value** val );
int pure_map_iter_has_next( const struct pure_map* map , int cursor );
int pure_map_iter_deref( const struct pure_map* map , int cursor , const char** key , struct pure_value** val );

#ifdef __cplusplus
}
#endif /*__cplusplus */

#endif /* PURE_H_ */