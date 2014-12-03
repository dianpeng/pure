Pure,a lightweight configuration script
==================================
# Introduction
Pure is a simple , lightweight and self contained library that provides better configuration function for application.
Configuration has been long time dominated by Json/XML/Yaml, however since these formats are just data storage format,
it lacks the ability to do simple operations. Many software do need the functional operation feature in its configuration
file, like NGINX webserver, the configuration file format is based on Json, however it supports some sort of runtime operations.
This blur boundary between a real functional script and a simple compact data storage format makes me decide to create a 
_JUST_WORK_ script library that is specifically target for configuration. It should be small and lightweight but powerful enough 
for different program to describe its configuration data.

# What's the feature ?
1) Built-in JSON like data structure

2) Function invocation, definition and recursive function

3) C function extension

4) Simple and intuitive API

5) Supports while/for/foreach/if else if else control structure

6) C-like syntax 

7) Automatic garbage collection by reference count

8) Small library size (with -Os , gcc generates a 28KB library)


# Sample

## Array
```
# Test if the input value is a number or not
func IsOdd(num) {
	if( num % 2 == 0 ) 
		return 0;
	else
		return 1;
}

# Filter out the odd number in the array
func FilterOutOdd(arr) {
	ret = [];
	for( i , arr ) {
		if( IsOdd(i) ) {
			arr_add(ret,i);
		}
	}
	return ret;
}

# Print the array out
func PrintArray(arr) {
	for(ele,arr) {
		print(ele);
	}
}

# Main
OddArr = FilterOutOdd([1,2,3,4,5,6,7,8,9,10]);
PrintArray(OddArr);
```
## Map
```
SimpleMap = {"Key1":"Value1","Key2":"Value2","Key3":1234,"Key4":[1,2,3,4,5]};
print(SimpleMap["Key1"]);
print(SimpleMap["Key2"]);
SimpleMap["Key2"] = nil;
print(SimpleMap["Key2"]);
```

# Performance
Pure implements a _ONE_ pass lexing/parsing/interpretation phase. This is OK for configuration, because user just 
need to grab the final value for each item inside of the configuration. However, Pure is _NOT_ a good candidate for general 
script, especially some part of the code will be executed multiple times. A Fibonacci recursive implementation in Pure will be
_HUNDREDS_ times slower than a native implementation in C. However, if the execution flow is just one time pass , then Pure's speed
is way more than enough and even better than some famous script language.

# Build
1. Make
2. VS

#License
MIT


