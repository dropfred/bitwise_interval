#ifndef BITWISE_INTERVAL_H
#define BITWISE_INTERVAL_H

#include <type_traits>
#include <climits>
#include <algorithm>
// #include <bit>
// #include <concepts>

// template <std::integral T>
template <typename T>
struct Interval
{
    T low, high;
    T step;

    Interval(T low, T high, T step = T(1)) : low(low), high(high), step(step) {}

    Interval() : Interval(T(0), T(0)) {}

    template <typename X>
    Interval(Interval<X> const & i) : Interval(T(i.low), T(i.high), T(i.step)) {}
};

template <typename T>
Interval<T> not_interval(Interval<T> const & i)
{
    return {T(~i.high), T(~i.low), i.step};
}

template <typename X, typename Y>
auto and_interval(Interval<X> const & x, Interval<Y> const & y)
{
	using CT = std::common_type_t<X, Y>;

	return and_interval(Interval<CT> {x}, Interval<CT> {y});
}

template <typename T>
T first(T v, T step, T rem)
{
    T r = v % step;
    if (r <= rem)
    {
        v += rem - r;
    }
    else
    {
        v += step - (r - rem);
    }
    return v;
}

template <typename T>
T last(T v, T step, T rem)
{
    T r = v % step;
    if (r >= rem)
    {
        v -= (r - rem);
    }
    else
    {
        v = (v - step) + (rem - r);
    }
    return v;
}

template <typename T>
Interval<T> and_interval(Interval<T> const & x, Interval<T> const & y)
{
    constexpr T zero = T(0);
    constexpr T one  = T(1);
    constexpr T msb  = T(one << (sizeof (T) * CHAR_BIT - 1U));

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using UI = Interval<UT>;
        if ((x.high < zero) || (x.low >= zero))
        {
            if ((y.high < zero) || (y.low >= zero))
            {
                auto r = and_interval
                (
                    UI {UT(x.low), UT(x.high), UT(x.step)},
                    UI {UT(y.low), UT(y.high), UT(y.step)}
                );
                return {T(r.low), T(r.high), T(r.step)};
            }
            else
            {
                return and_interval(y, x);
            }
        }
        else
        {
            if (y.low >= 0)
            {
                // TODO: step
                return {T(0), y.high};
            }
            else if (y.high < zero)
            {
                // TODO: step
                auto n = and_interval(UI {UT(x.low), UT(-one)}, UI {UT(y.low), UT(y.high)});
                auto p = and_interval(UI {UT(zero), UT(x.high)}, UI {UT(y.low), UT(y.high)});
                return {T(n.low), T(p.high)};
            }
            else
            {
                // TODO: step
                auto n = and_interval(UI {UT(x.low), UT(-one)}, UI {UT(y.low), UT(-one)});
                auto p1 = and_interval(UI {UT(x.low), UT(-one)}, UI {UT(zero), UT(y.high)});
                auto p2 = and_interval(UI {UT(zero), UT(x.high)}, UI {UT(y.low), UT(-one)});
                auto p3 = and_interval(UI {UT(zero), UT(x.high)}, UI {UT(zero), UT(y.high)});
                return
                {
                    T(n.low),
                    T(std::max({p1.high, p2.high, p3.high}))
                };
            }
        }
    }
    else
    {
        // constexpr T zero = 0U;
        // constexpr T one  = 1U;
        // constexpr T msb  = one << (sizeof (T) * CHAR_BIT - 1U);

        T low  = zero;
        T high = zero;

        // T step_x = std::has_single_bit(x.step) ? x.step : one;
        // T step_y = std::has_single_bit(y.step) ? y.step : one;
        T step_x = ((x.step & (x.step - one)) == zero) ? x.step : one;
        T step_y = ((y.step & (y.step - one)) == zero) ? y.step : one;

        T mask_x = ~(step_x - one);
        T mask_y = ~(step_y - one);

        T rem_x = ~mask_x & x.low;
        T rem_y = ~mask_y & y.low;

        T step = ((step_x > step_y) && (rem_x == zero)) ? step_x
               : ((step_y > step_x) && (rem_y == zero)) ? step_y
               : std::min(step_x, step_y);

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        for (T b = msb; b != zero; b >>= one)
        {
            T r = b - one;

            // low
            {
                bool flip_x = ((low_x.low ^ low_x.high) & b) != zero;
                bool flip_y = ((low_y.low ^ low_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        // low_x & low_y done
                        low_x.low = low_x.high = rem_x;
                        low_y.low = low_y.high = rem_y;
                    }
                    else
                    {
                        if ((low_y.high & b) == zero)
                        {
                            // low_x done
                            low_x.low = low_x.high = rem_x;
                        }
                        else
                        {
                            low_x.high = (mask_x & r) | rem_x;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        if ((low_x.high & b) == zero)
                        {
                            // low_y done
                            low_y.low = low_y.high = rem_y;
                        }
                        else
                        {
                            low_y.high = (mask_y & r) | rem_y;
                        }
                    }
                    else
                    {
                        low |= (low_x.low & low_y.low) & b;
                    }
                }
            }

            // high
            {
                bool flip_x = ((high_x.low ^ high_x.high) & b) != zero;
                bool flip_y = ((high_y.low ^ high_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        high |= b;
                        high_x.low = rem_x;
                        high_y.low = rem_y;
                    }
                    else
                    {
                        if ((high_y.high & b) == zero)
                        {
                            // high_x done
                            high_x.low = high_x.high = (mask_x & r) | rem_x;
                        }
                        else
                        {
                            high |= b;
                            high_x.low = rem_x;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        if ((high_x.high & b) == zero)
                        {
                            // high_y done
                            high_y.low = high_y.high = (mask_y & r) | rem_y;
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
        }

        return {low, high, step};
    }
}

template <typename T>
Interval<T> or_interval(Interval<T> const & x, Interval<T> const & y)
{
    return not_interval(and_interval(not_interval(x), not_interval(y)));
}

// KO : The `or` trick above doesn't work with `xor`.
// template <typename T>
// Interval<T> xor_interval(Interval<T> const & x, Interval<T> const & y)
// {
//     return or_interval
//     (
//        and_interval(x, not_interval(y)),
//        and_interval(not_interval(x), y)
//     );
// }

template <typename T>
Interval<T> xor_interval(Interval<T> const & x, Interval<T> const & y)
{
    constexpr T zero = T(0);
    constexpr T one  = T(1);
    constexpr T msb  = T(one << (sizeof (T) * CHAR_BIT - 1U));

    if constexpr (std::is_signed_v<T>)
    {
        // using UT = std::make_unsigned_t<T>;
        // using UI = Interval<UT>;
        // if ((x.high < zero) || (x.low >= zero))
        // {
        //     if ((y.high < zero) || (y.low >= zero))
        //     {
        //         auto r = xor_interval
        //         (
        //             UI {UT(x.low), UT(x.high), UT(x.step)},
        //             UI {UT(y.low), UT(y.high), UT(y.step)}
        //         );
        //         return {T(r.low), T(r.high), T(r.step)};
        //     }
        //     else
        //     {
        //         return xor_interval(y, x);
        //     }
        // }
        // else
        // {
        //     if (y.low >= 0)
        //     {
        //         // TODO: step
        //         return {T(0), y.high};
        //     }
        //     else if (y.high < zero)
        //     {
        //         // TODO: step
        //         auto n = xor_interval(UI {UT(x.low), UT(-one)}, UI {UT(y.low), UT(y.high)});
        //         auto p = xor_interval(UI {UT(zero), UT(x.high)}, UI {UT(y.low), UT(y.high)});
        //         return {T(n.low), T(p.high)};
        //     }
        //     else
        //     {
        //         // TODO: step
        //         auto n = xor_interval(UI {UT(x.low), UT(-one)}, UI {UT(y.low), UT(-one)});
        //         auto p1 = xor_interval(UI {UT(x.low), UT(-one)}, UI {UT(zero), UT(y.high)});
        //         auto p2 = xor_interval(UI {UT(zero), UT(x.high)}, UI {UT(y.low), UT(-one)});
        //         auto p3 = xor_interval(UI {UT(zero), UT(x.high)}, UI {UT(zero), UT(y.high)});
        //         return
        //         {
        //             T(n.low),
        //             T(std::max({p1.high, p2.high, p3.high}))
        //         };
        //     }
        // }
        return {T(0), T(0)};
    }
    else
    {
        T low  = zero;
        T high = zero;

        // T step_x = std::has_single_bit(x.step) ? x.step : one;
        // T step_y = std::has_single_bit(y.step) ? y.step : one;
        T step_x = ((x.step & (x.step - one)) == zero) ? x.step : one;
        T step_y = ((y.step & (y.step - one)) == zero) ? y.step : one;

        // TODO
        T step = std::min(step_x, step_y);
        step_x = step_y = step;

        T mask_x = ~(step_x - one);
        T mask_y = ~(step_y - one);

        T rem_x = ~mask_x & x.low;
        T rem_y = ~mask_y & y.low;

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        for (T b = msb; b != zero; b >>= one)
        {
            T r = b - one;

            // low
            {
                bool flip_x = ((low_x.low ^ low_x.high) & b) != zero;
                bool flip_y = ((low_y.low ^ low_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        low_x.low = low_x.high = rem_x;
                        low_y.low = low_y.high = rem_y;
                    }
                    else
                    {
                        if ((low_y.high & b) == zero)
                        {
                            low_x.high = (mask_x & r) | rem_x;
                        }
                        else
                        {
                            low_x.low = rem_x;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        if ((low_x.high & b) == zero)
                        {
                            low_y.high = (mask_y & r) | rem_y;
                        }
                        else
                        {
                            low_y.low = rem_y;
                        }
                    }
                    else
                    {
                        low |= (low_x.low ^ low_y.low) & b;
                    }
                }
            }

            // high
            {
                bool flip_x = ((high_x.low ^ high_x.high) & b) != zero;
                bool flip_y = ((high_y.low ^ high_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        high |= b;
                        T m = std::min(mask_x, mask_y);
                        high_x.low = rem_x;
                        high_x.high = rem_x;
                        high_y.low = (r & m) | rem_y;
                        high_y.high = (r & m) | rem_y;
                    }
                    else
                    {
                        high |= b;
                        if ((high_y.high & b) == zero)
                        {
                            high_x.low = rem_x;
                        }
                        else
                        {
                            high_x.high = (mask_x & r) | rem_x;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        high |= b;
                        if ((high_x.high & b) == zero)
                        {
                            high_y.low = rem_y;
                        }
                        else
                        {
                            high_y.high = (mask_y & r) | rem_y;
                        }
                    }
                    else
                    {
                        high |= (high_x.low ^ high_y.low) & b;
                    }
                }
            }
        }

        return {low, high, step};
    }
}

// TODO step
template <typename T>
Interval<T> xor_interval_1(Interval<T> const & x, Interval<T> const & y)
{
    if constexpr (std::is_signed_v<T>)
    {
        // TODO : signed
        return {T(0), T(0)};
    }
    else
    {
        constexpr T zero = 0U;
        constexpr T one  = 1U;
        constexpr T msb  = one << (sizeof (T) * CHAR_BIT - 1U);

        T low  = zero;
        T high = zero;

        T step = one;

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        bool low_done = false, high_done = false;

        for (T b = msb; b != zero; b >>= one)
        {
            T r = b - one;

            // low
            if (!low_done)
            {
                bool flip_x = ((low_x.low ^ low_x.high) & b) != zero;
                bool flip_y = ((low_y.low ^ low_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        // low_x.low = low_x.high = zero;
                        // low_y.low = low_y.high = zero;

                        low_done = true;
                    }
                    else
                    {
                        if ((low_y.high & b) == zero)
                        {
                            low_x.high = r;
                        }
                        else
                        {
                            low_x.low = 0;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        if ((low_x.high & b) == zero)
                        {
                            low_y.high = r;
                        }
                        else
                        {
                            low_y.low = 0;
                        }
                    }
                    else
                    {
                        low |= (low_x.low ^ low_y.high) & b;
                    }
                }
            }

            // high
            if (!high_done)
            {
                bool flip_x = ((high_x.low ^ high_x.high) & b) != zero;
                bool flip_y = ((high_y.low ^ high_y.high) & b) != zero;

                if (flip_x)
                {
                    if (flip_y)
                    {
                        // high |= b;
                        // high_x.low = high_x.high = zero;
                        // high_y.low = high_y.high = r;
                        // // or
                        // // high_x.low = high_x.high = r;
                        // // high_y.low = high_y.high = zero;

                        high |= b | r;
                        high_done = true;

                    }
                    else
                    {
                        high |= b;
                        if ((high_y.high & b) == zero)
                        {
                            high_x.low = zero;
                        }
                        else
                        {
                            high_x.high = r;
                        }
                    }
                }
                else
                {
                    if (flip_y)
                    {
                        high |= b;
                        if ((high_x.high & b) == zero)
                        {
                            high_y.low = zero;
                        }
                        else
                        {
                            high_y.high = r;
                        }
                    }
                    else
                    {
                        high |= (high_x.low ^ high_y.high) & b;
                    }
                }
            }

            if (low_done && high_done) break;
        }

        return {low, high, step};
    }
}

#endif
