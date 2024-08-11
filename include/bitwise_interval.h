#ifndef BITWISE_INTERVAL_H
#define BITWISE_INTERVAL_H

#include <type_traits>
#include <climits>
#include <algorithm>
#include <numeric>
// #include <bit>
// #include <concepts>
#include <cassert>

template <typename T>
auto uabs(T n)
{
    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        if (n >= 0)
        {
            return UT(n);
        }
        else
        {
            return UT(UT(~n) + 1U);
        }
    }
    else
    {
        return n;
    }
}


template <typename T>
auto umod(T n, std::make_unsigned_t<T> d)
{
    auto m = uabs(n) % d;
    if constexpr (std::is_signed_v<T>)
    {
        if ((m != 0) && (n < 0))
        {
            m = d - m;
        }
    }
    return std::make_unsigned_t<T>(m);
}

template <typename T>
T round_up(T v, T step, T rem)
{
    T r = umod(v, step);
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
T round_down(T v, T step, T rem)
{
    T r = umod(v, step);
    if (r >= rem)
    {
        v -= r - rem;
    }
    else
    {
        v -= (step - rem) + r;
    }
    return v;
}

// template <std::integral T>
template <typename T>
struct Interval
{
    using Type = T;
    using UType = std::make_unsigned_t<T>;

    // TODO
    // Type low, high;
    // UType step;

    T low, high;
    T step;
    
    // left unitialized
    Interval() {}

    template <typename X>
    Interval(X low, X high, X step = X(1)) : low(T(low)), high(T(high)), step(T(step))
    {
        assert(this->step != 0);
        assert((this->low != this->high) || (this->step == T(1))); // ???
        assert(this->low <= this->high);
        assert(umod(this->low, this->step) == umod(this->high, this->step));
    }

    template <typename X>
    Interval(Interval<X> const & i) : Interval(i.low, i.high, i.step) {}

    Interval(Interval const &) = default;
    Interval(Interval &&) = default;

    Interval & operator = (Interval const &) = default;

    template <typename X>
    Interval & operator = (Interval<X> const & i)
    {
        return (*this = Interval {i});
    }

    Interval sub(T min, T max) const
    {
        assert((min >= low) && (max <= high));
        T rem = umod(low, step);
        return {round_up(min, step, rem), round_down(max, step, rem), step};
    }
};

template <typename T>
Interval<T> not_interval(Interval<T> const & i)
{
    return {T(~i.high), T(~i.low), i.step};
}

// template <typename X, typename Y>
// auto and_interval(Interval<X> const & x, Interval<Y> const & y)
// {
// 	using CT = std::common_type_t<X, Y>;

// 	return and_interval(Interval<CT> {x}, Interval<CT> {y});
// }

template <typename T>
Interval<T> and_interval(Interval<T> const & x, Interval<T> const & y)
{
    constexpr T zero = T(0);
    constexpr T one  = T(1);

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
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
            if (y.low >= 0)
            {
                // UI nx {x.low, last(T(-one), x.step, umod(x.low, x.step)), x.step};
                // UI px {first(zero, x.step, umod(x.low, x.step)), x.high, x.step};
                // UI n = and_interval(nx, UI {y});
                // UI p = and_interval(px, UI {y});
                // return {std::min(n.low, p.low), std::max(n.high, p.high), n.step};

                UI n = and_interval(UI {x.sub(x.low, m_one)}, UI {y});
                UI p = and_interval(UI {x.sub(zero, x.high)}, UI {y});
                // assert(n.step == p.step);
                return {std::min(n.low, p.low), std::max(n.high, p.high), std::min(n.step, p.step)};
            }
            else if (y.high < zero)
            {
                // T last_x  = last(T(-one), x.step, umod(x.low, x.step));
                // T first_x = first(zero, x.step, umod(x.low, x.step));
                // auto n = and_interval(UI {x.low, last_x, x.step}, UI {y.low, y.high, y.step});
                // auto p = and_interval(UI {first_x, x.high, x.step}, UI {y.low, y.high, y.step});
                // assert(n.step == p.step);
                // return {n.low, p.high, n.step};

                UI n = and_interval(UI {x.sub(x.low, m_one)}, UI {y});
                UI p = and_interval(UI {x.sub(zero, x.high)}, UI {y});
                // assert(n.step == p.step);
                return {n.low, p.high, std::min(n.step, p.step)};
            }
            else
            {
                // T last_x  = last(T(-one), x.step, umod(x.low, x.step));
                // T first_x = first(zero, x.step, umod(x.low, x.step));
                // T last_y  = last(T(-one), y.step, umod(y.low, y.step));
                // T first_y = first(zero, y.step, umod(y.low, y.step));
                // auto n  = and_interval(UI {x.low, last_x, x.step}, UI {y.low, last_y, y.step});
                // auto p1 = and_interval(UI {x.low, last_x, x.step}, UI {first_y, y.high, y.step});
                // auto p2 = and_interval(UI {first_x, x.high, x.step}, UI {y.low, last_y, y.step});
                // auto p3 = and_interval(UI {first_x, x.high, x.step}, UI {first_y, y.high, y.step});
                // assert((n.step == p1.step) && (n.step == p2.step) && (n.step == p3.step));
                // return {n.low, std::max({p1.high, p2.high, p3.high}), n.step};

                auto nx = x.sub(x.low, m_one);
                auto px = x.sub(zero, x.high);
                auto ny = y.sub(y.low, m_one);
                auto py = y.sub(zero, y.high);

                auto n  = and_interval(UI {nx}, UI {ny});
                auto p1 = and_interval(UI {nx}, UI {py});
                auto p2 = and_interval(UI {px}, UI {ny});
                auto p3 = and_interval(UI {px}, UI {py});
                // assert((n.step == p1.step) && (n.step == p2.step) && (n.step == p3.step));
                return {n.low, std::max({p1.high, p2.high, p3.high}), std::max({n.step, p1.step, p2.step, p3.step})};
            }
        }
    }
    else
    {
        constexpr T msb  = one << (sizeof (T) * CHAR_BIT - 1U);

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
#if 0
    constexpr T zero = T(0);
    constexpr T one  = T(1);

    if constexpr (std::is_signed_v<T>)
    {
        using UT = std::make_unsigned_t<T>;
        using UI = Interval<UT>;

        if ((x.low >= zero) || (x.high < zero))
        {
            if ((y.low >= zero) || (y.high < zero))
            {
                return or_interval(UI {x}, UI {y});
            }
            else
            {
                return or_interval(y, x);
            }
        }
        else
        {
            constexpr T m_one = T(-1);

            // TODO: step
            if ((y.low >= 0) || (y.high < zero))
            {
                UI nx = or_interval(UI {x.sub(x.low, m_one)}, UI {y});
                UI px = or_interval(UI {x.sub(zero, x.high)}, UI {y});
                assert(nx.step == px.step);
                return {std::min(nx.low, px.low), std::max(nx.high, px.high), nx.step};
            }
            else
            {
                auto nx = x.sub(x.low, m_one);
                auto px = x.sub(zero, x.high);
                auto ny = y.sub(y.low, m_one);
                auto py = y.sub(zero, y.high);

                auto i1 = or_interval(UI {nx}, UI {ny});
                auto i2 = or_interval(UI {nx}, UI {py});
                auto i3 = or_interval(UI {px}, UI {ny});
                auto i4 = or_interval(UI {px}, UI {py});
                assert((i1.step == i2.step) && (i2.step == i3.step) && (i3.step == i4.step));
                return
                {
                    std::min({i1.low , i2.low , i3.low , i4.low}),
                    std::max({i1.high, i2.high, i3.high, i4.high}),
                    i1.step
                };
            }
        }
    }
    else
#endif
    {
        return not_interval(and_interval(not_interval(x), not_interval(y)));
    }
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
                I nx {xor_interval(UI {x.sub(x.low, m_one)}, UI {y})};
                I px {xor_interval(UI {x.sub(zero, x.high)}, UI {y})};
                // assert(nx.step == px.step);
                return {std::min(nx.low, px.low), std::max(nx.high, px.high), std::min(nx.step, px.step)};
            }
            else
            {
                I nx = x.sub(x.low, m_one);
                I px = x.sub(zero, x.high);
                I ny = y.sub(y.low, m_one);
                I py = y.sub(zero, y.high);

                I nn {xor_interval(UI {nx}, UI {ny})};
                I np {xor_interval(UI {nx}, UI {py})};
                I pn {xor_interval(UI {px}, UI {ny})};
                I pp {xor_interval(UI {px}, UI {py})};
                // assert((nn.step == np.step) && (np.step == pn.step) && (pn.step == pp.step));
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

#if 0
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

#endif
