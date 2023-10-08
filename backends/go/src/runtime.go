package main

import "fmt"

type __wpl_type_enumeration struct {
	__wpl_enumeration_discriminant uint
	__wpl_enumeration_payload      any
}

func __wpl_intrinsic_display(s string) struct{} {
	fmt.Println(s)
	return struct{}{}
}

func __wpl_intrinsic_number_to_text(n float64) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_natural_to_text(n uint64) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_add_number(a float64, b float64) float64 {
	return a + b
}

func __wpl_intrinsic_number_equality(a float64, b float64) __wpl_type_enumeration {
	var discriminant uint
	if a == b {
		discriminant = 1
	} else {
		discriminant = 0
	}

	return __wpl_type_enumeration{discriminant, struct{}{}}
}

func __wpl_intrinsic_natural_equality(a uint64, b uint64) __wpl_type_enumeration {
	var discriminant uint
	if a == b {
		discriminant = 1
	} else {
		discriminant = 0
	}

	return __wpl_type_enumeration{discriminant, struct{}{}}
}

func __wpl_intrinsic_natural_ordering(a uint64, b uint64) __wpl_type_enumeration {
	var discriminant uint
	if a < b {
		discriminant = 0
	} else if a == b {
		discriminant = 1
	} else {
		discriminant = 2
	}

	return __wpl_type_enumeration{discriminant, struct{}{}}
}

func __wpl_intrinsic_add_natural(a uint64, b uint64) uint64 {
	return a + b
}

func __wpl_intrinsic_subtract_natural(a uint64, b uint64) uint64 {
	return a - b
}

func __wpl_intrinsic_make_mutable[T any](x T) *T {
	return &x
}

func __wpl_intrinsic_get_mutable[T any](x *T) T {
	return *x
}

func __wpl_intrinsic_set_mutable[T any](x *T, v T) struct{} {
	*x = v
	return struct{}{}
}
