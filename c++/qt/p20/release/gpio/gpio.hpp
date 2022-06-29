
#ifndef GPIO_H
#define GPIO_H

/*
  gpio.hpp
  My gpio simulator class

  @author Danny
  @version 1.0 04/25/2020
 */
#include <ctime>
#include <vector>
#include <cstdio>
#include "mutex.hpp"

typedef bool bit_t;
typedef unsigned int uint;

// GPio helper macros
#define is_pin_valid(pin) ((pin) > -1)
#define is_pin_set(gpio, pin) ((gpio)->getPin(pin) == 1)
#define is_pin_clear(gpio, pin) (!is_pin_set(gpio, pin))

#define loop_until_set(gpio, pin) while(is_pin_clear(gpio, pin))
#define loop_until_cleared(gpio, pin) while(is_pin_set(gpio, pin))

#define set_pin(gpio, pin) ((gpio)->setPinTo(pin, 1))
#define clear_pin(gpio, pin) ((gpio)->setPinTo(pin, 0))

#define TIME_OUT 1000 // 1us in nanoseconds

class GPio
{
public:
    // Constructors
    GPio() : GPio(20) {}
    GPio(uint num_pins)
        : pins(std::vector<mutex<bit_t>*>(num_pins)),
          capacity(num_pins)
        {
            for (uint i = 0; i < num_pins; ++i) {
                pins[i] = new mutex<bit_t>(0);
            }
            tim = (timespec*)calloc(1, sizeof(timespec));
            tim->tv_nsec = TIME_OUT;
        }
    // Returns a shared instance of GPio
    static GPio* getInstance()
        {
            static GPio instance; // Guaranteed to be destroyed.
            return &instance;
        }
    ~GPio() {
        for (uint i = 0; i < this->capacity; ++i) {
            delete pins[i];
        }
    }
    // Return the value held in the GPio pin
    bit_t getPin(uint pinNum) {
        if (pinNum < capacity) return pins[pinNum]->get();
        else {
            printf("error: accessing non-existent pin number %d\n", pinNum);
            return -1;
        }
    }
    // Set the value held in the GPio pin to 'bit'
    void setPinTo(uint pinNum, bit_t bit) {
        if (pinNum < capacity) pins[pinNum]->set(bit);
        else printf("error: setting non-existent pin number %d\n", pinNum);
    }
    //
    // This function is used to prevent 100% cpu usage
    // when using getPin in a while(true) loop
    int sleep() {
        // On successfully sleeping for the requested interval
        // nanosleep() returns 0
        return nanosleep(tim, NULL) == 0;
    }
private:
    std::vector<mutex<bit_t>*> pins;
    uint capacity = 0;
    // [[deprecated("timer removed due to no usage")]]
    struct timespec *tim; // time out struct
};

#endif // GPIO_H