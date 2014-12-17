#include <pure.h>
#include <assert.h>
#include <stdio.h>

const char* example1 = "example1.pure";
const char* example2 = "example2.pure";
const char* example3 = "example3.pure";

int run_example123() {
    struct pure* p = pure_create();
    int line,pos;
    int ec;
    const char* str;

    if( pure_run_file(p,example3,&str,&ec,&line,&pos) !=0 ) {
        fprintf(stderr,"Error in script:%s at location:(%d:%d)",str,line,pos);
        return -1;
    }
    return 0;
}

static
int print_val( const char* title, const struct pure_value* val );

static
int print_array( const struct pure_value* val ) {
    struct pure_array* arr;
    struct pure_value* v;
    size_t i;

    assert(val->type== PURE_ARRAY);
    if( pure_value_get_arr(val,&arr) != 0 )
        return -1;

    for( i = 0 ; i < pure_array_size(arr) ; ++i ) {
        if( pure_array_index(arr,i,&v) !=0 )
            return -1;
        print_val(NULL,v);
    }
    return 0;
}

static
int print_map( const struct pure_value* val ) {
    struct pure_map* map;
    int cursor;
    struct pure_value* v;
    const char* k;
    assert(val->type == PURE_MAP);
    if( pure_value_get_map(val,&map) != 0 )
        return -1;
    cursor = pure_map_iter_start(map,&k,&v);
    while( pure_map_iter_has_next(map,cursor) ) {
        printf("Key:%s",k);
        print_val("Val",v);
        /* move to the next position */
        cursor = pure_map_iter_deref(map,cursor,&k,&v);
    }
    return 0;
}

static
int print_val( const char* title, const struct pure_value* val ) {
    const char* str;
    double num;

    if( title != NULL )
        printf("%s:",title);
    switch(val->type) {
    case PURE_STRING:
        if( pure_value_get_str(val,&str) != 0 )
            return -1;
        printf("%s",str);
        break;
    case PURE_NUMBER:
        if( pure_value_get_num(val,&num) != 0 )
            return -1;
        printf("%f",num);
        break;
    case PURE_NIL:
        printf("<nil>");
        break;
    case PURE_ARRAY:
        print_array(val);
        break;
    case PURE_MAP:
        print_map(val);
        break;
    case PURE_USER_DATA:
        printf("<userdata>");
        break;
    default: break;
    }
    printf("\n");
    return 0;
}

int run_example4() {
    struct pure* p = pure_create();
    struct pure_value* val;
    int line,pos;
    int ec;
    const char* str;
    if( pure_run_file(p,"example4.pure",&str,&ec,&line,&pos) != 0 ) {
        fprintf(stderr,"Error in script:%s at location:(%d:%d)",str,line,pos);
        return -1;
    }
    /* retreive the data from the pure */
    if( pure_get(p,"AppWidth",&val) != 0 ) {
        return -1;
    }
    print_val("AppWidth",val);
    if( pure_get(p,"AppHeight",&val) !=0 ) {
        return -1;
    }
    print_val("AppHeight",val);
    if( pure_get(p,"UserList",&val) != 0 ) {
        return -1;
    }
    print_val("UserList",val);
    if( pure_get(p,"OddArray",&val) !=0 ) {
        return -1;
    }
    print_val("OddArray",val);
    printf("Done");

    return 0;
}

int main() {
    printf("From host program!\n");
    run_example123();
    return 0;
}
