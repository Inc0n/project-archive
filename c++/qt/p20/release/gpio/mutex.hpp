#ifndef _MUTEX_H
#define _MUTEX_H

/*
  mutex.hpp
  My mutex wrapper class, that wraps mutex and the data together into one object

  @author Danny
  @version 1.0 04/25/2020
 */

#include <mutex>

template <typename T>
class mutex
{
public:
    mutex() {}
    mutex(T defaultVal) { data = defaultVal; }
    // gettors
    T get() {
        m.lock();
        T copy = data;
        m.unlock();
        return copy;
    }
    /**
       Returns a reference to the data
       @note: to use this function in particular,
       it should be locked before performing anything that has side effects on it
    */
    // T& getRef() {
    //     return data;
    // }
    void set(T x) {
        m.lock();
        data = x;
        m.unlock();
    }
    // void lock() { m.lock(); }
    // void unlock() { m.unlock(); }
private:
    std::mutex m;
    T data;
};

#endif // _MUTEX_H
