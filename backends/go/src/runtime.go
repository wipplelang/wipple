package main

import (
	"fmt"
	"math"
	"math/rand"
	"sync"
	"time"

	"github.com/manifoldco/promptui"
	"github.com/shopspring/decimal"
)

type __wpl_type_number = decimal.Decimal

type __wpl_type_enumeration struct {
	__wpl_enumeration_discriminant uint
	__wpl_enumeration_payload      any
}

type __wpl_type_ui struct{}

type __wpl_type_taskgroup struct {
	mutex *sync.Mutex
	tasks *[]func()
}

func __wpl_intrinsic_crash[T any](s string) T {
	panic(fmt.Sprintf("error: %s", s))
}

func __wpl_intrinsic_display(s string) struct{} {
	fmt.Println(s)
	return struct{}{}
}

func __wpl_intrinsic_prompt[T any](prompt string, parse func(input string) __wpl_type_enumeration) (value T) {
	config := promptui.Prompt{
		Label: prompt,
		Validate: func(input string) error {
			parsed := parse(input)

			if parsed.__wpl_enumeration_discriminant == 0 {
				return fmt.Errorf("invalid input")
			} else {
				value = parsed.__wpl_enumeration_payload.(struct{ __wpl_variant_element_0 T }).__wpl_variant_element_0

				return nil
			}
		},
	}

	_, err := config.Run()

	if err != nil {
		panic(fmt.Sprintf("error: %v", err))
	}

	return value
}

func __wpl_intrinsic_choice(prompt string, descriptions []string) uint64 {
	panic("choice is not yet supported")
}

func __wpl_intrinsic_with_ui[T any](url string, callback func(handle __wpl_type_ui) T) T {
	panic("with-ui is only supported in the playground")
}

func __wpl_intrinsic_message_ui[T any, U any](handle __wpl_type_ui, message string, value T) U {
	panic("message-ui is only supported in the playground")
}

func __wpl_intrinsic_with_continuation[T any](f func(func(result T) struct{}) struct{}) T {
	ch := make(chan T, 1)

	f(func(result T) struct{} {
		ch <- result
		return struct{}{}
	})

	return <-ch
}

func __wpl_intrinsic_with_task_group(f func(__wpl_type_taskgroup) struct{}) struct{} {
	taskGroup := __wpl_type_taskgroup{
		mutex: &sync.Mutex{},
		tasks: &[]func(){},
	}

	f(taskGroup)

	taskGroup.mutex.Lock()
	ch := make(chan struct{}, len(*taskGroup.tasks))
	for _, body := range *taskGroup.tasks {
		go func(body func()) {
			body()
			ch <- struct{}{}
		}(body)
	}
	for range *taskGroup.tasks {
		<-ch
	}
	taskGroup.mutex.Unlock()

	return struct{}{}
}

func __wpl_intrinsic_task(taskGroup __wpl_type_taskgroup, body func(struct{}) struct{}) {
	taskGroup.mutex.Lock()
	*taskGroup.tasks = append(*taskGroup.tasks, func() {
		body(struct{}{})
	})
	taskGroup.mutex.Unlock()
}

func __wpl_intrinsic_in_background(body func(struct{}) struct{}) {
	go body(struct{}{})
}

func __wpl_intrinsic_delay(ms uint64) struct{} {
	time.Sleep(time.Duration(ms) * time.Millisecond)
	return struct{}{}
}

func __wpl_intrinsic_number_to_text(n __wpl_type_number) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_integer_to_text(n int64) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_natural_to_text(n uint64) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_byte_to_text(n uint8) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_signed_to_text(n int) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_unsigned_to_text(n uint) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_float_to_text(n float32) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_double_to_text(n float64) string {
	return fmt.Sprintf("%v", n)
}

func __wpl_intrinsic_text_to_number(s string) *__wpl_type_number {
	d, err := decimal.NewFromString(s)
	if err != nil {
		return nil
	}

	return &d
}

func __wpl_intrinsic_add_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Add(b)
}

func __wpl_intrinsic_subtract_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Sub(b)
}

func __wpl_intrinsic_multiply_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Mul(b)
}

func __wpl_intrinsic_divide_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Div(b)
}

func __wpl_intrinsic_modulo_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Mod(b)
}

func __wpl_intrinsic_power_number(a __wpl_type_number, b __wpl_type_number) __wpl_type_number {
	return a.Pow(b)
}

func __wpl_intrinsic_negate_number(n __wpl_type_number) __wpl_type_number {
	return n.Neg()
}

func __wpl_intrinsic_add_integer(a int64, b int64) int64 {
	return a + b
}

func __wpl_intrinsic_subtract_integer(a int64, b int64) int64 {
	return a - b
}

func __wpl_intrinsic_multiply_integer(a int64, b int64) int64 {
	return a * b
}

func __wpl_intrinsic_divide_integer(a int64, b int64) int64 {
	return a / b
}

func __wpl_intrinsic_modulo_integer(a int64, b int64) int64 {
	return a % b
}

func __wpl_intrinsic_power_integer(a int64, b int64) int64 {
	return int64(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_negate_integer(n int64) int64 {
	return -n
}

func __wpl_intrinsic_add_natural(a uint64, b uint64) uint64 {
	return a + b
}

func __wpl_intrinsic_subtract_natural(a uint64, b uint64) uint64 {
	return a - b
}

func __wpl_intrinsic_multiply_natural(a uint64, b uint64) uint64 {
	return a * b
}

func __wpl_intrinsic_divide_natural(a uint64, b uint64) uint64 {
	return a / b
}

func __wpl_intrinsic_modulo_natural(a uint64, b uint64) uint64 {
	return a % b
}

func __wpl_intrinsic_power_natural(a uint64, b uint64) uint64 {
	return uint64(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_add_byte(a uint8, b uint8) uint8 {
	return a + b
}

func __wpl_intrinsic_subtract_byte(a uint8, b uint8) uint8 {
	return a - b
}

func __wpl_intrinsic_multiply_byte(a uint8, b uint8) uint8 {
	return a * b
}

func __wpl_intrinsic_divide_byte(a uint8, b uint8) uint8 {
	return a / b
}

func __wpl_intrinsic_modulo_byte(a uint8, b uint8) uint8 {
	return a % b
}

func __wpl_intrinsic_power_byte(a uint8, b uint8) uint8 {
	return uint8(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_add_signed(a int, b int) int {
	return a + b
}

func __wpl_intrinsic_subtract_signed(a int, b int) int {
	return a - b
}

func __wpl_intrinsic_multiply_signed(a int, b int) int {
	return a * b
}

func __wpl_intrinsic_divide_signed(a int, b int) int {
	return a / b
}

func __wpl_intrinsic_modulo_signed(a int, b int) int {
	return a % b
}

func __wpl_intrinsic_power_signed(a int, b int) int {
	return int(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_negate_signed(n int) int {
	return -n
}

func __wpl_intrinsic_add_unsigned(a uint, b uint) uint {
	return a + b
}

func __wpl_intrinsic_subtract_unsigned(a uint, b uint) uint {
	return a - b
}

func __wpl_intrinsic_multiply_unsigned(a uint, b uint) uint {
	return a * b
}

func __wpl_intrinsic_divide_unsigned(a uint, b uint) uint {
	return a / b
}

func __wpl_intrinsic_modulo_unsigned(a uint, b uint) uint {
	return a % b
}

func __wpl_intrinsic_power_unsigned(a uint, b uint) uint {
	return uint(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_add_float(a float32, b float32) float32 {
	return a + b
}

func __wpl_intrinsic_subtract_float(a float32, b float32) float32 {
	return a - b
}

func __wpl_intrinsic_multiply_float(a float32, b float32) float32 {
	return a * b
}

func __wpl_intrinsic_divide_float(a float32, b float32) float32 {
	return a / b
}

func __wpl_intrinsic_modulo_float(a float32, b float32) float32 {
	return float32(math.Mod(float64(a), float64(b)))
}

func __wpl_intrinsic_power_float(a float32, b float32) float32 {
	return float32(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_negate_float(n float32) float32 {
	return -n
}

func __wpl_intrinsic_add_double(a float64, b float64) float64 {
	return a + b
}

func __wpl_intrinsic_subtract_double(a float64, b float64) float64 {
	return a - b
}

func __wpl_intrinsic_multiply_double(a float64, b float64) float64 {
	return a * b
}

func __wpl_intrinsic_divide_double(a float64, b float64) float64 {
	return a / b
}

func __wpl_intrinsic_modulo_double(a float64, b float64) float64 {
	return math.Mod(a, b)
}

func __wpl_intrinsic_power_double(a float64, b float64) float64 {
	return float64(math.Pow(float64(a), float64(b)))
}

func __wpl_intrinsic_negate_double(n float64) float64 {
	return -n
}

func __wpl_intrinsic_number_equality(a __wpl_type_number, b __wpl_type_number) __wpl_type_enumeration {
	var discriminant uint
	if a.Equal(b) {
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

func __wpl_intrinsic_text_equality(a string, b string) __wpl_type_enumeration {
	var discriminant uint
	if a == b {
		discriminant = 1
	} else {
		discriminant = 0
	}

	return __wpl_type_enumeration{discriminant, struct{}{}}
}

func __wpl_intrinsic_text_characters(s string) []string {
	chars := []string{}
	for _, c := range s {
		chars = append(chars, string(c))
	}

	return chars
}

func __wpl_intrinsic_number_ordering(a __wpl_type_number, b __wpl_type_number) __wpl_type_enumeration {
	discriminant := uint(a.Cmp(b) + 1)
	return __wpl_type_enumeration{discriminant, struct{}{}}
}

func __wpl_intrinsic_integer_ordering(a int64, b int64) __wpl_type_enumeration {
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

func __wpl_intrinsic_natural_to_number(n uint64) __wpl_type_number {
	return decimal.NewFromInt(int64(n))
}

func __wpl_intrinsic_random_natural(from uint64, to uint64) uint64 {
	return uint64(rand.Int63n(int64(to-from)) + int64(from))
}

func __wpl_intrinsic_random_integer(from int64, to int64) int64 {
	return int64(rand.Intn(int(to-from)) + int(from))
}

func __wpl_intrinsic_random_number(from __wpl_type_number, to __wpl_type_number) __wpl_type_number {
	panic("TODO")
}

func __wpl_intrinsic_make_mutable[T any](x T) *T {
	return &x
}

func __wpl_intrinsic_get_mutable[T any](x *T) T {
	return *x
}

func __wpl_intrinsic_set_mutable[T any](x *T, v T) struct{} {
	// FIXME: Use a wrapper type with a sync.Mutex
	*x = v
	return struct{}{}
}

// __wpl_intrinsic_make_empty_list is implemented inline

func __wpl_intrinsic_list_first[T any](list []T) *T {
	if len(list) > 0 {
		return &list[0]
	} else {
		return nil
	}
}

func __wpl_intrinsic_list_last[T any](list []T) *T {
	if len(list) > 0 {
		return &list[len(list)-1]
	} else {
		return nil
	}
}

func __wpl_intrinsic_list_initial[T any](list []T) *[]T {
	if len(list) > 0 {
		initial := list[0:]
		return &initial
	} else {
		return nil
	}
}

func __wpl_intrinsic_list_tail[T any](list []T) *[]T {
	if len(list) > 0 {
		tail := list[:len(list)-1]
		return &tail
	} else {
		return nil
	}
}

func __wpl_intrinsic_list_nth[T any](list []T, index uint64) *T {
	if len(list) > int(index) {
		return &list[index]
	} else {
		return nil
	}
}

func __wpl_intrinsic_list_append[T any](list []T, value T) []T {
	new := make([]T, len(list)+1)

	for index, element := range list {
		new[index] = element
	}

	new[len(new)-1] = value

	return new
}

func __wpl_intrinsic_list_prepend[T any](list []T, value T) []T {
	new := make([]T, len(list)+1)

	for index, element := range list {
		new[index+1] = element
	}

	new[0] = value

	return new
}

func __wpl_intrinsic_list_insert_at[T any](list []T, target uint64, value T) *[]T {
	if int(target) > len(list) {
		return nil
	}

	new := make([]T, len(list)+1)

	for index, element := range list {
		if index < int(target) {
			new[index] = element
		} else {
			new[index+1] = element
		}
	}

	new[target] = value

	return &new
}

func __wpl_intrinsic_list_remove_at[T any](list []T, target uint64) *T {
	if int(target) > len(list) {
		return nil
	}

	new := make([]T, len(list)-1)

	for index, element := range list {
		if index < int(target) {
			new[index] = element
		} else {
			new[index-1] = element
		}
	}

	value := list[target]

	return &value
}

func __wpl_intrinsic_list_count[T any](list []T) uint64 {
	return uint64(len(list))
}
