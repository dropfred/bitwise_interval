#ifndef BITWISE_INTERVAL_H
#define BITWISE_INTERVAL_H

#include <type_traits>
#include <algorithm>
#include <numeric>
#include <climits>
#include <functional>
#include <bit>
#include <cassert>

//
// utils
//

template <typename T>
constexpr T msb = T(std::make_unsigned_t<T>(1) << (sizeof (T) * CHAR_BIT - 1));

template <typename UT>
requires std::is_unsigned_v<UT>
auto get_step2(UT n)
{
    return UT(UT(1) << std::countr_zero(n));
}

template <typename T>
auto distance(T a, T b)
{
    using UT = std::make_unsigned_t<T>;

    return (a >= 0) ? UT(b - a)
         : (b <  0) ? UT(UT(b) - UT(a))
         :            UT(UT(~a) + 1U + UT(b));
}

template <typename T>
auto uabs(T n)
{
    using UT = std::make_unsigned_t<T>;
    UT un = UT(n);
    if constexpr (std::is_signed_v<T>)
    {
        if (n < 0)
        {
            un = ~un + 1;
        }
    }
    return un;
}

template <typename T, typename UT>
requires std::is_same_v<UT, std::make_unsigned_t<T>>
auto umod(T n, UT d)
{
    UT m = d;
    if (d != 0)
    {
        m = uabs(n) % d;
        if constexpr (std::is_signed_v<T>)
        {
            if (n < 0)
            {
                m = (d - m) % d;
            }
        }
    }
    return m;
}

template <typename T>
struct Interval
{
    using Type = T;
    using UType = std::make_unsigned_t<T>;

    Type low, high;
    UType step;

    static Interval const Empty;

    Interval() : Interval(Type(0), Type(0), msb<UType>) {}

    template <typename I, typename UI>
    requires std::is_integral_v<I> && std::is_same_v<UI, std::make_unsigned_t<I>>
    Interval(I low, I high, UI step = 1) : low(Type(low)), high(Type(high)), step(UType(step))
    {
        assert(low <= high);
        if (low != high)
        {
            assert(step != 0);
            assert(umod(low, step) == umod(high, step));
        }
        else
        {
            assert(is_singleton() || is_empty());
        }
    }

    template <typename X>
    Interval(Interval<X> const & i) : Interval(Type(i.low), Type(i.high), UType(i.step)) {}

    Interval(Interval const &) = default;
    Interval(Interval &&) = default;

    Interval & operator = (Interval const &) = default;
    Interval & operator = (Interval &&) = default;

    template <typename X>
    Interval & operator = (Interval<X> const & i)
    {
        return (*this = Interval {i});
    }

    Type round_up(Type v) const
    {
        assert(!is_empty() && (v >= low));
        if (!is_singleton())
        {
            UType r  = umod(low, step);
            UType sr = umod(v  , step);
            if (sr <= r)
            {
                v += r - sr;
            }
            else
            {
                v += step - (sr - r);
            }
        }
        return v;
    }

    Type round_down(Type v) const
    {
        assert(!is_empty() && (v <= high));
        if (!is_singleton())
        {
            UType r  = umod(low, step);
            UType sr = umod(v  , step);
            if (sr >= r)
            {
                v -= sr - r;
            }
            else
            {
                v -= (step - r) + sr;
            }
        }
        return v;
    }

    Interval sub(Type low, Type high) const
    {
        assert(!is_empty());
        Type rl = round_up(low);
        Type rh = round_down(high);
        UType s = (rl == rh) ? UType(0) : step;
        return {rl, rh, s};
    }

    bool is_singleton() const
    {
        return (step == 0);
    }

    bool is_empty() const
    {
        return (*this == Interval::Empty);
    }
};

template <typename T>
Interval<T> const Interval<T>::Empty {};

template <typename T>
using IntervalBinaryOp = std::function<Interval<T> (Interval<T> const &, Interval<T> const &)>;

template <typename T, typename F>
requires std::is_convertible_v<F, IntervalBinaryOp<T>>
Interval<T> interval_fold(std::initializer_list<Interval<T>> intervals, F f)
{
    assert(intervals.size() > 0);
    auto i = intervals.begin(), ie = intervals.end();
    Interval<T> m = *i++;
    while (i != ie)
    {
        m = f(m, *i++);
    }
    return m;
}

template <typename T>
Interval<T> interval_union(Interval<T> const & x, Interval<T> const & y)
{
    if (x.is_empty())
    {
        return y; 
    }
    if (y.is_empty())
    {
        return x; 
    }

    using UT = std::make_unsigned_t<T>;

    T low  = std::min(x.low , y.low );
    T high = std::max(x.high, y.high);
    UT step;

    if (x.is_singleton() && y.is_singleton())
    {
        step = distance(low, high);
    }
    else
    {
        step = x.is_singleton() ? y.step
             : y.is_singleton() ? x.step
             : std::gcd(x.step, y.step);
        UT rx = umod(x.low, step);
        UT ry = umod(y.low, step);
        if (rx != ry)
        {
            UT dr = (rx < ry) ? distance(rx, ry) : distance(ry, rx);
            step = std::gcd(step, dr);
        }
    }

    return {low, high, step};
}

template <typename T>
Interval<T> interval_union(std::initializer_list<Interval<T>> intervals)
{
    return interval_fold(intervals, [] (Interval<T> const & x, Interval<T> const & y) {return interval_union(x, y);});
}

template <typename T>
Interval<T> interval_not(Interval<T> const & i)
{
    if (i.is_empty())
    {
        return i;
    }
    return {T(~i.high), T(~i.low), i.step};
}

template <typename T>
Interval<T> interval_and(Interval<T> const & x, Interval<T> const & y)
{
    if (x.is_empty())
    {
        return y; 
    }
    if (y.is_empty())
    {
        return x; 
    }

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using I  = Interval<T>;
        using UI = Interval<UT>;

        // signs
        bool s_x = (x.low < 0) && (x.high >= 0);
        bool s_y = (y.low < 0) && (y.high >= 0);

        if (s_x && s_y)
        {
            I nx = x.sub(x.low, T(-1) );
            I px = x.sub(T(0) , x.high);
            I ny = y.sub(y.low, T(-1) );
            I py = y.sub(T(0) , y.high);

            I nn = interval_and(UI {nx}, UI {ny});
            I np = interval_and(UI {nx}, UI {py});
            I pn = interval_and(UI {px}, UI {ny});
            I pp = interval_and(UI {px}, UI {py});

            return interval_union({nn, np, pn, pp});
        }
        else if (s_x)
        {
            I nx = interval_and(UI {x.sub(x.low, T(-1)) }, UI {y});
            I px = interval_and(UI {x.sub(T(0) , x.high)}, UI {y});

            return interval_union(nx, px);
        }
        else if (s_y)
        {
            return interval_and(y, x);
        }
        else
        {
            return interval_and(UI {x}, UI {y});
        }
    }
    else
    {
        T low  = 0;
        T high = 0;

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        T step;
        T step_x = get_step2(x.step);
        T step_y = get_step2(y.step);

        {
            T f = (~(step_x - 1) | x.low) & (~(step_y - 1) | y.low);
            step = step = std::min(T(step_x - 1), T(step_y - 1)) + 1;
            while (((step & f) == 0) && (step != 0))
            {
                step <<= 1;
                T fr = ~(step - 1);
                if (((x.low ^ x.high) & fr) == 0)
                {
                    f &= x.low;
                    step = std::max(T(step - 1), T(step_y - 1)) + 1;
                }
                if (((y.low ^ y.high) & fr) == 0)
                {
                    f &= y.low;
                    step = std::max(T(step - 1), T(step_x - 1)) + 1;
                }
            }
        }

        T flip_x = ~(step_x - 1);
        T flip_y = ~(step_y - 1);

        T rem_x = ~flip_x & x.low;
        T rem_y = ~flip_y & y.low;

        for (T b = msb<T>; b != 0; b >>= 1)
        {
            T rem = b - 1;

            // low
            {
                bool f_x = ((low_x.low ^ low_x.high) & b) != 0;
                bool f_y = ((low_y.low ^ low_y.high) & b) != 0;

                if (f_x && f_y)
                {
                    // low_x & low_y done
                    low_x.low = low_x.high = rem_x;
                    low_y.low = low_y.high = rem_y;
                }
                else if (f_x)
                {
                    if ((low_y.high & b) == 0)
                    {
                        // low_x done
                        low_x.low = low_x.high = rem_x;
                    }
                    else
                    {
                        low_x.high = (flip_x & rem) | rem_x;
                    }

                }
                else if (f_y)
                {
                    if ((low_x.high & b) == 0)
                    {
                        // low_y done
                        low_y.low = low_y.high = rem_y;
                    }
                    else
                    {
                        low_y.high = (flip_y & rem) | rem_y;
                    }
                }
                else
                {
                    low |= (low_x.low & low_y.low) & b;
                }
            }

            // high
            {
                bool f_x = ((high_x.low ^ high_x.high) & b) != 0;
                bool f_y = ((high_y.low ^ high_y.high) & b) != 0;

                if (f_x && f_y)
                {
                    high |= b;
                    high_x.low = rem_x;
                    high_y.low = rem_y;
                }
                else if (f_x)
                {
                    if ((high_y.high & b) == 0)
                    {
                        // high_x done
                        high_x.low = high_x.high = (flip_x & rem) | rem_x;
                    }
                    else
                    {
                        high |= b;
                        high_x.low = rem_x;
                    }
                }
                else if (f_y)
                {
                    if ((high_x.high & b) == 0)
                    {
                        // high_y done
                        high_y.low = high_y.high = (flip_y & rem) | rem_y;
                    }
                    else
                    {
                        high |= b;
                        high_y.low = rem_y;
                    }
                }
                else
                {
                    high |= (high_x.low & high_y.low) & b;
                }
            }
        }

        if (low == high)
        {
            step = 0;
        }

        return {low, high, step};
    }
}

template <typename T>
Interval<T> interval_and(std::initializer_list<Interval<T>> intervals)
{
    return interval_fold(intervals, [] (Interval<T> const & x, Interval<T> const & y) {return interval_and(x, y);});
}

template <typename T>
Interval<T> interval_or(Interval<T> const & x, Interval<T> const & y)
{
    return interval_not(interval_and(interval_not(x), interval_not(y)));
}

template <typename T>
Interval<T> interval_or(std::initializer_list<Interval<T>> intervals)
{
    return interval_fold(intervals, [] (Interval<T> const & x, Interval<T> const & y) {return interval_or(x, y);});
}

template <typename T>
Interval<T> interval_xor(Interval<T> const & x, Interval<T> const & y)
{
    // not sure what's going on here: boot methods give correct results,
    // but sometimes one is better than the other.
    // return interval_or
    // (
    //     interval_and(x, interval_not(y)),
    //     interval_and(interval_not(x), y)
    // );
    // return interval_and
    // (
    //     interval_not(interval_and(x, y)),
    //     interval_not(interval_and(interval_not(x), interval_not(y)))
    // );
    auto x1 = interval_or
    (
        interval_and(x, interval_not(y)),
        interval_and(interval_not(x), y)
    );
    auto x2 = interval_and
    (
        interval_not(interval_and(x, y)),
        interval_not(interval_and(interval_not(x), interval_not(y)))
    );
    return {std::max(x1.low, x2.low), std::min(x1.high, x2.high), std::max(x1.step, x2.step)};
}

template <typename T>
Interval<T> interval_xor(std::initializer_list<Interval<T>> intervals)
{
    return interval_fold(intervals, [] (Interval<T> const & x, Interval<T> const & y) {return interval_xor(x, y);});
}

template <typename T>
Interval<T> operator ~ (Interval<T> const & i)
{
    return interval_not(i);
}

template <typename T>
Interval<T> operator & (Interval<T> const & x, Interval<T> const & y)
{
    return interval_and(x, y);
}

template <typename T>
Interval<T> operator | (Interval<T> const & x, Interval<T> const & y)
{
    return interval_or(x, y);
}

template <typename T>
Interval<T> operator ^ (Interval<T> const & x, Interval<T> const & y)
{
    return interval_xor(x, y);
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator & (Interval<T> const & x, I y)
{
    using UI = std::make_unsigned_t<I>;
    return interval_and(x, Interval<T> {y, y, UI(0)});
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator | (Interval<T> const & x, I y)
{
    using UI = std::make_unsigned_t<I>;
    return interval_or(x, Interval<T> {y, y, UI(0)});
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator ^ (Interval<T> const & x, I y)
{
    using UI = std::make_unsigned_t<I>;
    return interval_xor(x, Interval<T> {y, y, UI(0)});
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator & (I x, Interval<T> const & y)
{
    return (y & x);
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator | (I x, Interval<T> const & y)
{
    return (y | x);
}

template <typename T, typename I>
requires std::is_integral_v<I>
Interval<T> operator ^ (I x, Interval<T> const & y)
{
    return (y ^ x);
}

template <typename T>
bool operator == (Interval<T> const & a, Interval<T> const & b)
{
    return ((a.low == b.low) && (a.high == b.high) && (a.step == b.step));
}

template <typename T>
bool operator != (Interval<T> const & a, Interval<T> const & b)
{
    return !(a == b);
}

template <typename T>
bool operator >= (Interval<T> const & a, Interval<T> const & b)
{
    return
    (
        (a.low <= b.low) &&
        (a.high >= b.high) &&
        (b.is_singleton() || ((a.step <= b.step) && (std::gcd(a.step, b.step) == a.step))) &&
        (umod(a.low, a.step) == umod(b.low, a.step))
    );
}

template <typename T>
bool operator <= (Interval<T> const & a, Interval<T> const & b)
{
    return (b >= a);
}

template <typename T>
bool operator < (Interval<T> const & a, Interval<T> const & b)
{
    return ((a <= b) && (a != b));
}

template <typename T>
bool operator > (Interval<T> const & a, Interval<T> const & b)
{
    return (b < a);
}

// TODO
template <typename T, typename I>
Interval<T> operator << (Interval<T> const & i, I s)
{
    using UT = std::make_unsigned_t<T>;
    T  low  = i.low  << s;
    T  high = i.high << s;
    UT step = i.step << s;
    return {std::min(low, high), std::max(low, high), step};
}

// TODO
template <typename T, typename I>
Interval<T> operator >> (Interval<T> const & i, I s)
{
    using UT = std::make_unsigned_t<T>;
    T low   = i.low  >> s;
    T high  = i.high >> s;
    UT step = get_step2(i.step);
    if (low == high)
    {
        step = 0;
    }
    else if (std::countr_zero(step) >= s)
    {
        step >>= s;
    }
    else
    {
        step = 1;
    }
    return {low, high, step};
}

#endif
