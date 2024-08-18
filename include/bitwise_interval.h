#ifndef BITWISE_INTERVAL_H
#define BITWISE_INTERVAL_H

#include <type_traits>
#include <algorithm>
#include <numeric>
#include <climits>
#include <cassert>

// utils
template <typename T>
auto uabs(T n)
{
    using UT = std::make_unsigned_t<T>;
    UT un = UT(n);
    if constexpr (std::is_signed_v<T>)
    {
        if (n < 0)
        {
            un = ~un + 1U;
        }
    }
    return un;
}

template <typename T, typename UT>
auto umod(T n, UT d)
{
    static_assert(std::is_unsigned_v<UT>);
    auto m = uabs(n);
    m %= d;
    if constexpr (std::is_signed_v<T>)
    {
        if ((m != 0U) && (n < 0))
        {
            m = d - m;
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

    // left unitialized ?
    Interval() {}

    Interval(Type low, Type high, UType step = 1U) : low(low), high(high), step(step)
    {
        assert(this->step != 0U);
        assert(this->low <= this->high);
        assert(umod(this->low, this->step) == umod(this->high, this->step));
        // assert((this->low != this->high) || (this->step == 1U));
        if (this->low == this->high)
        {
            this->step = 1U;
        }
    }

    // For signed / unsigned conversion.
    template <typename X>
    Interval(Interval<X> const & i) : Interval(Type(i.low), Type(i.high), UType(i.step)) {}

    Interval(Interval const &) = default;
    Interval(Interval &&) = default;

    Interval & operator = (Interval const &) = default;

    template <typename X>
    Interval & operator = (Interval<X> const & i)
    {
        return (*this = Interval {i});
    }

    Type round_up(Type v) const
    {
        UType r  = umod(low, step);
        UType sr = umod(v, step);
        if (sr <= r)
        {
            v += r - sr;
        }
        else
        {
            v += step - (sr - r);
        }
        return v;
    }

    Type round_down(Type v) const
    {
        UType r  = umod(low, step);
        UType sr = umod(v, step);
        if (sr >= r)
        {
            v -= sr - r;
        }
        else
        {
            v -= (step - r) + sr;
        }
        return v;
    }


    Interval sub(T low, T high) const
    {
        assert((low >= this->low) && (high <= this->high));
        return {round_up(low), round_down(high), step};
    }
};

template <typename T>
Interval<T> not_interval(Interval<T> const & i)
{
    return {T(~i.high), T(~i.low), i.step};
}

template <typename T>
Interval<T> and_interval(Interval<T> const & x, Interval<T> const & y)
{
    constexpr T zero = T(0);
    constexpr T one  = T(1);

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using I  = Interval<T>;
        using UI = Interval<UT>;

        if ((x.low >= zero) || (x.high < zero))
        {
            if ((y.low >= zero) || (y.high < zero))
            {
                return and_interval(UI {x}, UI {y});
            }
            else
            {
                return and_interval(y, x);
            }
        }
        else
        {
            constexpr T m_one = T(-1);

            // TODO: step
            if ((y.low >= 0) || (y.high < zero))
            {
                I nx = and_interval(UI {x.sub(x.low, m_one)}, UI {y});
                I px = and_interval(UI {x.sub(zero, x.high)}, UI {y});
                return {std::min(nx.low, px.low), std::max(nx.high, px.high), std::min(nx.step, px.step)};
            }
            else
            {
                I nx = x.sub(x.low, m_one);
                I px = x.sub(zero, x.high);
                I ny = y.sub(y.low, m_one);
                I py = y.sub(zero, y.high);

                I nn = and_interval(UI {nx}, UI {ny});
                I np = and_interval(UI {nx}, UI {py});
                I pn = and_interval(UI {px}, UI {ny});
                I pp = and_interval(UI {px}, UI {py});
                return
                {
                    std::min({nn.low , np.low , pn.low , pp.low }),
                    std::max({nn.high, np.high, pn.high, pp.high}),
                    std::min({nn.step, np.step, pn.step, pp.step})
                };
            }
        }
    }
    else
    {
        constexpr T msb  = one << (sizeof (T) * CHAR_BIT - 1U);

        T low  = zero;
        T high = zero;

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        // T step_x = std::has_single_bit(x.step) ? x.step : one;
        // T step_y = std::has_single_bit(y.step) ? y.step : one;
        T step_x = ((x.step & (x.step - one)) == zero) ? x.step : one;
        T step_y = ((y.step & (y.step - one)) == zero) ? y.step : one;

        T mask_x = ~(step_x - one);
        T mask_y = ~(step_y - one);

        T rem_x = ~mask_x & x.low;
        T rem_y = ~mask_y & y.low;

        T step = ((step_x > step_y) && ((rem_x == zero) || (y.low == y.high))) ? step_x
               : ((step_y > step_x) && ((rem_y == zero) || (x.low == x.high))) ? step_y
               : std::min(step_x, step_y);


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

        if (low == high)
        {
            step = one;
        }

        return {low, high, step};
    }
}

template <typename T>
Interval<T> or_interval(Interval<T> const & x, Interval<T> const & y)
{
    return not_interval(and_interval(not_interval(x), not_interval(y)));
}

template <typename T>
Interval<T> xor_interval(Interval<T> const & x, Interval<T> const & y)
{
    constexpr T zero = T(0);
    constexpr T one  = T(1);

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using I  = Interval<T>;
        using UI = Interval<UT>;

        if ((x.low >= zero) || (x.high < zero))
        {
            if ((y.low >= zero) || (y.high < zero))
            {
                return xor_interval(UI {x}, UI {y});
            }
            else
            {
                return xor_interval(y, x);
            }
        }
        else
        {
            constexpr T m_one = T(-1);

            // TODO: step
            if ((y.low >= 0) || (y.high < zero))
            {
                I nx = xor_interval(UI {x.sub(x.low, m_one)}, UI {y});
                I px = xor_interval(UI {x.sub(zero, x.high)}, UI {y});
                return {std::min(nx.low, px.low), std::max(nx.high, px.high), std::min(nx.step, px.step)};
            }
            else
            {
                I nx = x.sub(x.low, m_one);
                I px = x.sub(zero, x.high);
                I ny = y.sub(y.low, m_one);
                I py = y.sub(zero, y.high);

                I nn = xor_interval(UI {nx}, UI {ny});
                I np = xor_interval(UI {nx}, UI {py});
                I pn = xor_interval(UI {px}, UI {ny});
                I pp = xor_interval(UI {px}, UI {py});
                return
                {
                    std::min({nn.low , np.low , pn.low , pp.low }),
                    std::max({nn.high, np.high, pn.high, pp.high}),
                    std::min({nn.step, np.step, pn.step, pp.step})
                };
            }
        }
    }
    else
    {
        constexpr T msb  = T(one << (sizeof (T) * CHAR_BIT - 1U));

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

        if (low == high)
        {
            step = one;
        }

        return {low, high, step};
    }
}

#endif
