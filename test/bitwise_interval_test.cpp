#include <bitwise_interval.h>

#include <iostream>
#include <string>
#include <map>
#include <functional>
#include <stdint.h>
#include <bitset>
#include <array>
#include <random>
#include <limits>
#include <cassert>

namespace
{
    template <typename T> struct Dbg
    {
        T value;
        Dbg(T value) : value(value) {}
    };

    template <typename T>
    std::ostream & operator << (std::ostream & os, Dbg<T> const & v)
    {
        if constexpr (sizeof (T) == 1)
        {
            os << std::bitset<CHAR_BIT>((unsigned long long)(v.value));
        }
        else
        {
            os << std::hex << std::uppercase << v.value;
        }
        return os;
    }

    template <typename T>
    bool operator == (Interval<T> const & a, Interval<T> const & b)
    {
        return ((a.low == b.low) && (a.high == b.high));
    }

    template <typename T>
    bool operator >= (Interval<T> const & a, Interval<T> const & b)
    {
        return ((a.low <= b.low) && (a.high >= b.high));
    }

    template <typename T>
    std::ostream & operator << (std::ostream & os, Interval<T> const & i)
    {
        os << '[' << Dbg {i.low} << ", " << Dbg  {i.high} << "]";
        if (i.step > 1)
        {
            os << "/" << (+i.step);
        }
        return os;
    }

    template <typename T>
    T parse(std::string const & str)
    {
        if ((str.size() > 2) && ((str.find("0b") == 0) || (str.find("0B") == 0)))
        {
            return T(std::stoull(str.c_str() + 2, nullptr, 2));
        }
        return T(std::stoull(str, nullptr, 0));
    }

    template <typename T>
    bool test_interval(Interval<T> const x, Interval<T> const y, bool trace = true)
    {
        auto c_and = and_interval(x, y);
        auto c_or  = or_interval(x, y);
        auto c_xor = xor_interval(x, y);

        struct
        {
            T rem;
            bool ok = true;
        } r_and {T(c_and.low % c_and.step)}, r_or {T(c_or.low % c_or.step)}, r_xor {T(c_xor.low % c_xor.step)};

        Interval<T> b_and, b_or, b_xor;

        b_and.low  = b_or.low  = b_xor.low  = std::numeric_limits<T>::max();
        b_and.high = b_or.high = b_xor.high = std::numeric_limits<T>::min();

        for (T i = x.low; ; i += x.step)
        {
            for (T j = y.low; ; j += y.step)
            {
                T r;

                r = i & j;
                if (r < b_and.low)
                {
                    b_and.low = r;
                }
                if (r > b_and.high)
                {
                    b_and.high = r;
                }
                if ((r % c_and.step) != r_and.rem)
                {
                    r_and.ok = false;
                }

                r = i | j;
                if (r < b_or.low)
                {
                    b_or.low = r;
                }
                if (r > b_or.high)
                {
                    b_or.high = r;
                }
                if ((r % c_or.step) != r_or.rem)
                {
                    r_or.ok = false;
                }

                r = i ^ j;
                if (r < b_xor.low)
                {
                    b_xor.low = r;
                }
                if (r > b_xor.high)
                {
                    b_xor.high = r;
                }
                if ((r % c_xor.step) != r_xor.rem)
                {
                    r_xor.ok = false;
                }

                if (j == y.high) break;
            }

            if (i == x.high) break;
        }

        bool ok = (c_and >= b_and) && r_and.ok/* && (c_or >= b_or) && r_or.ok && (c_xor >= b_xor) && r_xor.ok*/;

        if (!ok || trace)
        {
            auto ok = [] (Interval<T> const & a, Interval<T> const & b, bool r)
            {
                return
                (
                    r && (a == b) ? "OK " :
                    r && (a >= b) ? "OVER " :
                    (a == b)      ? "STEP " :
                    (a >= b)      ? "STEP+OVER" :
                                    "KO "
                );
            };
            std::cout << ok(c_and, b_and, r_and.ok) << x << " & " << y << " -> " << c_and << " : " << b_and << std::endl;
            // std::cout << ok(c_or , b_or , r_or.ok ) << x << " | " << y << " -> " << c_or  << " : " << b_or  << std::endl;
            // std::cout << ok(c_xor, b_xor, r_xor.ok) << x << " ^ " << y << " -> " << c_xor << " : " << b_xor << std::endl;
        }

        return ok;
    }

    template <typename T>
    void test(char const * args[], bool step)
    {
        Interval<T> i, j;

        try
        {
            char const ** a = args;
            i.low  = parse<T>(*a++);
            i.high = parse<T>(*a++);
            if (step) i.step = parse<T>(*a++);
            j.low  = parse<T>(*a++);
            j.high = parse<T>(*a++);
            if (step) j.step = parse<T>(*a++);
            assert
            (
                (i.low <= i.high) &&
                (j.low <= j.high) &&
                (i.step > 0) &&
                (j.step > 0)
            );
        }
        catch(std::exception const &)
        {
            std::cerr << "invalid number\n";
            return;
        }

        test_interval(i, j, true);
    }

    template <typename T>
    void test_all()
    {
        for (T low = std::numeric_limits<T>::min(); ; ++low)
        {
            //std::cout << "a.low : " << Dbg {low} << std::endl;
            for (T high = low; ; ++high)
            {
                Interval<T> a {low, high};
                std::cout << "a : " << a << std::endl;
                for (T low = std::numeric_limits<T>::min(); ; ++low)
                {
                    //std::cout << "b.low : " << Dbg {low} << std::endl;
                    for (T high = low; ; ++high)
                    {
                        //std::cout << "b.high : " << Dbg {high} << std::endl;
                        Interval<T> b {low, high};
                        //std::cout << "b : " << b << std::endl;
                        if (!test_interval(a, b, false))
                        {
                            return;
                        }
                        if (high == std::numeric_limits<T>::max()) break;
                    }
                    if (low == std::numeric_limits<T>::max()) break;
                }
                if (high == std::numeric_limits<T>::max()) break;
            }
            if (low == std::numeric_limits<T>::max()) break;
        }
    }

    template <typename T>
    class Rand
    {
        using D = std::uniform_int_distribution<T>;
        
        std::random_device rd {};

    public:

        T operator () ()
        {
            if constexpr (sizeof (T) == 1)
            {
                return T((Rand<short> {})(std::numeric_limits<T>::min(), std::numeric_limits<T>::max()));
            }
            return (D {std::numeric_limits<T>::min(), std::numeric_limits<T>::max()})(rd);
        }

        T operator () (T a, T b)
        {
            if constexpr (sizeof (T) == 1)
            {
                return T((Rand<short> {})(a, b));
            }
            return (D {a, b})(rd);
        }
    };

    template <typename T>
    void test_random()
    {
        Rand<T> rand {};

        constexpr T bits = (sizeof (T) * CHAR_BIT) - 1U;
        while (true)
        {
            T a_low = T(rand()); T a_high = rand(a_low, T(std::numeric_limits<T>::max()));
            T b_low = T(rand()); T b_high = rand(b_low, T(std::numeric_limits<T>::max()));

            T a_step = rand(1, bits), b_step = rand(1,  bits);

#if 0
            a_low &= 0xF; a_high &= 0xF;
            b_low &= 0xF; b_high &= 0xF;
            if (a_low > a_high) std::swap(a_low, a_high);
            if (b_low > b_high) std::swap(b_low, b_high);
            a_step &= 3; b_step &= 3;
            // a_step = b_step = 1U;
#endif

            a_high -= (a_high - a_low) % a_step;
            b_high -= (b_high - b_low) % b_step;

            if (!test_interval(Interval<T> {a_low, a_high, a_step}, Interval<T> {b_low, b_high, b_step}, true))
            {
                break;
            }
        }
    }
}

int main(int argc, char const * argv[])
{
    if (argc == 2)
    {
        static std::map<std::string, std::function<void (void)>> const tfs =
        {
            //{"s8" , test_all<int8_t>},
            //{"u8" , test_all<uint8_t>}
            {"s8" , test_random<int8_t>},
            {"u8" , test_random<uint8_t>},
            //{"s16", test_random<int16_t>},
            //{"s32", test_random<int32_t>},
            //{"s64", test_random<int64_t>},
            {"u16", test_random<uint16_t>},
            {"u32", test_random<uint32_t>},
            {"u64", test_random<uint64_t>},
            //{"s"  , test_random<int>},
            {"u"  , test_random<unsigned int>}
        };

        if (auto f = tfs.find(argv[1]); f != tfs.end())
        {
            f->second();
        }
        else
        {
            std::cerr << "invalid type '" << argv[1] << "'\n";
            return 1;
        }
    }
    else if ((argc == 6) || (argc == 8))
    {
        static std::map<std::string, std::function<void (char const **, bool)>> const tfs =
        {
            {"s8" , test<int8_t>},
            {"s16", test<int16_t>},
            {"s32", test<int32_t>},
            {"s64", test<int64_t>},
            {"u8" , test<uint8_t>},
            {"u16", test<uint16_t>},
            {"u32", test<uint32_t>},
            {"u64", test<uint64_t>},
            {"s"  , test<int>},
            {"u"  , test<unsigned int>}
        };

        if (auto f = tfs.find(argv[1]); f != tfs.end())
        {
            f->second(argv + 2, (argc == 8));
        }
        else
        {
            std::cerr << "invalid type '" << argv[1] << "'\n";
            return 1;
        }
    }
    else
    {
        std::cerr << "usage: " << argv[0] << "type [x.low x.high y.low y.high]\n";
        return 1;
    }

    return 0;
}
