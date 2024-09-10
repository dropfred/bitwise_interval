#ifndef BITWISE_INTERVAL_H
#define BITWISE_INTERVAL_H

#include <type_traits>
#include <algorithm>
#include <numeric>
#include <climits>
#include <cassert>

//
// utils
//

template <typename T>
constexpr T msb = T(std::make_unsigned_t<T>(1) << (sizeof (T) * CHAR_BIT - 1));

template <typename UT>
requires (std::is_unsigned_v<UT>)
auto get_step2(UT n)
{
    UT s;
    if ((n & (n - 1)) == 0)
    {
        s = n;
    }
    else
    {
        s = 1;
        while ((n & s) == 0)
        {
            s <<= 1;
        }
    }
    return s;
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
requires (std::is_same_v<UT, std::make_unsigned_t<T>>)
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

    // unitialized
    Interval() {}

    Interval(Type low, Type high, UType step = 1) : low(low), high(high), step(step)
    {
        assert(low <= high);
        if (low != high)
        {
            assert(step != 0);
            assert(umod(low, step) == umod(high, step));
        }
        else
        {
            this->step = 0;
        }
    }

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
        assert(v >= low);
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
        assert(v <= high);
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

    Interval sub(T low, T high) const
    {
        return {round_up(low), round_down(high), step};
    }

    bool is_singleton() const
    {
        return (step == 0);
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
    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using I  = Interval<T>;
        using UI = Interval<UT>;

        bool l_x = (x.low < 0) && (x.high >= 0);
        bool l_y = (y.low < 0) && (y.high >= 0);

        if (l_x && l_y)
        {
            I nx = x.sub(x.low, T(-1) );
            I px = x.sub(T(0) , x.high);
            I ny = y.sub(y.low, T(-1) );
            I py = y.sub(T(0) , y.high);

            I nn = and_interval(UI {nx}, UI {ny});
            I np = and_interval(UI {nx}, UI {py});
            I pn = and_interval(UI {px}, UI {ny});
            I pp = and_interval(UI {px}, UI {py});
            // return
            // {
            //     std::min({nn.low , np.low , pn.low , pp.low }),
            //     std::max({nn.high, np.high, pn.high, pp.high}),
            //     std::min({nn.step, np.step, pn.step, pp.step})
            // };
            return
            {
                std::min({nn.low , np.low , pn.low , pp.low }),
                std::max({nn.high, np.high, pn.high, pp.high})
            };
        }
        else if (l_x)
        {
            I nx = and_interval(UI {x.sub(x.low, T(-1)) }, UI {y});
            I px = and_interval(UI {x.sub(T(0) , x.high)}, UI {y});
            /*
            # x = [11111110 (-2), 00110010 (50)]/52
            # y = [01011101 (93), 01110101 (117)]/8
            Assertion failed: umod(this->low, this->step) == umod(this->high, this->step)
            */
            // return {std::min(nx.low, px.low), std::max(nx.high, px.high), std::min(nx.step, px.step)};
            return {std::min(nx.low, px.low), std::max(nx.high, px.high)};
        }
        else if (l_y)
        {
            return and_interval(y, x);
        }
        else
        {
            return and_interval(UI {x}, UI {y});
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
                // if ((x.low & fr) == (x.high & fr))
                if (((x.low ^ x.high) & fr) == 0)
                {
                    f &= x.low;
                     step = std::max(T(step - 1), T(step_y - 1)) + 1;
                }
                // if ((y.low & fr) == (y.high & fr))
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
            step = 1;
        }

        return {low, high, step};
    }
}

template <typename T>
Interval<T> or_interval(Interval<T> const & x, Interval<T> const & y)
{
    return not_interval(and_interval(not_interval(x), not_interval(y)));
}
#if 0
template <typename T>
Interval<T> xor_interval(Interval<T> const & x, Interval<T> const & y)
{
    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using I  = Interval<T>;
        using UI = Interval<UT>;

        bool l_x = (x.low < 0) && (x.high >= 0);
        bool l_y = (y.low < 0) && (y.high >= 0);

        if (l_x && l_y)
        {
            I nx = x.sub(x.low, T(-1) );
            I px = x.sub(T(0) , x.high);
            I ny = y.sub(y.low, T(-1) );
            I py = y.sub(T(0) , y.high);

            I nn = xor_interval(UI {nx}, UI {ny});
            I np = xor_interval(UI {nx}, UI {py});
            I pn = xor_interval(UI {px}, UI {ny});
            I pp = xor_interval(UI {px}, UI {py});
            // return
            // {
            //     std::min({nn.low , np.low , pn.low , pp.low }),
            //     std::max({nn.high, np.high, pn.high, pp.high}),
            //     std::min({nn.step, np.step, pn.step, pp.step})
            // };
            return
            {
                std::min({nn.low , np.low , pn.low , pp.low }),
                std::max({nn.high, np.high, pn.high, pp.high})
            };
        }
        else if (l_x)
        {
            I nx = xor_interval(UI {x.sub(x.low, T(-1) )}, UI {y});
            I px = xor_interval(UI {x.sub(T(0) , x.high)}, UI {y});
            // return {std::min(nx.low, px.low), std::max(nx.high, px.high), std::min(nx.step, px.step)};
            return {std::min(nx.low, px.low), std::max(nx.high, px.high)};
        }
        else if (l_y)
        {
            return xor_interval(y, x);
        }
        else
        {
            return xor_interval(UI {x}, UI {y});
        }
    }
    else
    {
        T low  = 0;
        T high = 0;

        T step_x = get_step2(x.step);
        T step_y = get_step2(y.step);

        // TODO
        T step = std::min(T(step_x - 1), T(step_y - 1)) + 1;

        T flip = ~(step - 1);

        T rem_x = ~flip & x.low;
        T rem_y = ~flip & y.low;

        auto low_x = x, high_x = x;
        auto low_y = y, high_y = y;

        for (T b = msb<T>; b != 0; b >>= 1)
        {
            T rem = b - 1;

            // low
            {
                bool f_x = ((low_x.low ^ low_x.high) & b) != 0;
                bool f_y = ((low_y.low ^ low_y.high) & b) != 0;

                if (f_x && f_y)
                {
                    low_x.low = low_x.high = rem_x;
                    low_y.low = low_y.high = rem_y;
                }
                else if (f_x)
                {
                    if ((low_y.high & b) == 0)
                    {
                        low_x.high = (flip & rem) | rem_x;
                    }
                    else
                    {
                        low_x.low = rem_x;
                    }
                }
                else if (f_y)
                {
                    if ((low_x.high & b) == 0)
                    {
                        low_y.high = (flip & rem) | rem_y;
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

            // high
            {
                bool f_x = ((high_x.low ^ high_x.high) & b) != 0;
                bool f_y = ((high_y.low ^ high_y.high) & b) != 0;

                if (f_x && f_y)
                {
                    high |= b;
                    high_x.low = high_x.high = rem_x;
                    high_y.low = high_y.high = (rem & flip) | rem_y;
                }
                else if (f_x)
                {
                    high |= b;
                    if ((high_y.high & b) == 0)
                    {
                        high_x.low = rem_x;
                    }
                    else
                    {
                        high_x.high = (flip & rem) | rem_x;
                    }
                }
                else if (f_y)
                {
                    high |= b;
                    if ((high_x.high & b) == 0)
                    {
                        high_y.low = rem_y;
                    }
                    else
                    {
                        high_y.high = (flip & rem) | rem_y;
                    }
                }
                else
                {
                    high |= (high_x.low ^ high_y.low) & b;
                }
            }
        }

        if (low == high)
        {
            step = 1;
        }

        return {low, high, step};
    }
}
#endif
template <typename T>
Interval<T> xor_interval(Interval<T> const & x, Interval<T> const & y)
{
    // return x;
    return or_interval
    (
        and_interval(x, not_interval(y)),
        and_interval(not_interval(x), y)
    );
}

#endif
