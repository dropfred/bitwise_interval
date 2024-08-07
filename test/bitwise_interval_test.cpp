#include <bitwise_interval.h>

#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <functional>
#include <stdint.h>
#include <bitset>
#include <array>
#include <random>
#include <limits>
#include <cassert>
#include <set>
#include <numeric>

#include  <cstdlib> // rand
// #include <format> // c++20

using namespace std::string_literals;

#define DEV_REPLAY_RAND

#ifndef INTERVAL_MAX
// #define INTERVAL_MAX 0x10000U
#define INTERVAL_MAX 0x1000U
#endif

#define BRUTE_STEP_MAX 1

namespace
{
    enum
    {
        S_1,
        S_PO2,
        S_ANY

    } STEP_MODE = S_ANY;

    bool DBG_HEX = false;

    template <typename T>
    bool operator == (Interval<T> const & a, Interval<T> const & b)
    {
        return ((a.low == b.low) && (a.high == b.high) && (a.step == b.step));
    }

    template <typename T>
    bool operator >= (Interval<T> const & a, Interval<T> const & b)
    {
        return ((a.low <= b.low) && (a.high >= b.high) && (a.step <= b.step) && (std::gcd(a.step, b.step) == a.step));
    }

    template <typename T> struct Dbg
    {
        T value;
        // bool force_decimal = false;
        Dbg(T value) : value(value) {}
    };

    template <typename T>
    std::ostream & operator << (std::ostream & os, Dbg<T> const & v)
    {
        if constexpr (sizeof (T) == 1)
        {
            if (DBG_HEX)
            {
                os << std::bitset<CHAR_BIT>((unsigned long long)(v.value)) << " (" << std::hex << std::uppercase << std::setw(2) << std::setfill('0') << (v.value & 0xFF) << ")";
            }
            else
            {
                os << std::bitset<CHAR_BIT>((unsigned long long)(v.value)) << " (" <<  std::dec << +v.value << ")";
            }
        }
        else
        {
            if (DBG_HEX)
            {
                os << std::hex << std::uppercase << std::setw(2 * sizeof (T) / CHAR_BIT) << std::setfill('0') << v.value;
            }
            else
            {
                os << std::dec << v.value;
            }
        }
        return os;
    }

    template <typename T>
    std::ostream & operator << (std::ostream & os, Interval<T> const & i)
    {
        os << '[' << Dbg {i.low} << ", " << Dbg  {i.high} << "]";
        if (i.step > 1)
        {
            os << "/" << std::dec << (+i.step);
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
    auto distance(T a, T b)
    {
        using UT = std::make_unsigned_t<T>;

        return (a >= 0) ? UT(b - a)
             : (b <  0) ? UT(UT(a) - UT(b))
             :            UT(UT(~a) + 1U + UT(b));
    }

#ifdef DEV_REPLAY_RAND
    class Rand
    {
    public:

        Rand()
        {
            std::srand(0);
        }

        template <typename T>
        T operator () (T a, T b)
        {
            double r = double(std::rand()) / RAND_MAX;

            return T((a * (1.0 - r)) + (b * r));
        }
    };

#else
    template <typename T>
    class Rand
    {
        using D = std::uniform_int_distribution<std::conditional_t<(sizeof (T) > 1), T, short int>>;
        
        std::random_device rd {};

    public:

        T operator () ()
        {
            return (*this)(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
        }

        T operator () (T a, T b)
        {
            return T((D {a, b})(rd));
        }
    };
#endif

    template <typename T>
    bool test_interval(Interval<T> const x, Interval<T> const y, bool trace = true)
    {
        std::cout << "# x = " << x << "\n# y = " << y << std::endl;

        auto c_not_x = not_interval(x);
        auto c_not_y = not_interval(y);

        Interval<T> b_not_x, b_not_y;
        b_not_x.low  = b_not_y.low  = std::numeric_limits<T>::max();
        b_not_x.high = b_not_y.high = std::numeric_limits<T>::min();
        b_not_x.step = c_not_x.step, b_not_y.step = c_not_y.step;

        for (T i = x.low; ; i += x.step)
        {
            T r;

            r = ~i;
            if (r < b_not_x.low)
            {
                b_not_x.low = r;
            }
            if (r > b_not_x.high)
            {
                b_not_x.high = r;
            }
            if (i == x.high) break;
        }

        for (T j = y.low; ; j += y.step)
        {
            T r;

            r = ~j;
            if (r < b_not_y.low)
            {
                b_not_y.low = r;
            }
            if (r > b_not_y.high)
            {
                b_not_y.high = r;
            }
            if (j == y.high) break;
        }

        auto c_and = and_interval(x, y);
        auto c_or  = or_interval(x, y);
        auto c_xor = xor_interval(x, y);

        struct
        {
            T rem;
            bool ok = true;
        } r_and {mod(c_and.low, c_and.step)}, r_or {mod(c_or.low, c_or.step)}, r_xor {mod(c_xor.low, c_xor.step)};

        Interval<T> b_and, b_or, b_xor;

        b_and.low  = b_or.low  = b_xor.low  = std::numeric_limits<T>::max();
        b_and.high = b_or.high = b_xor.high = std::numeric_limits<T>::min();

        constexpr bool BRUTE_STEP = sizeof (T) <= BRUTE_STEP_MAX;

        struct
        {
            std::set<T> vs;

            void add(T v)
            {
                if constexpr (BRUTE_STEP)
                {
                    vs.insert(v);
                }
            }

            std::size_t get_step() const
            {
                std::size_t s = 1;
                if (vs.size() > 1)
                {
                    auto j = vs.begin();
                    auto i = j++;
                    s = *j - *i;
                    for (i = j++; (j != vs.end()) && (s > 1); i = j++)
                    {
                        s = std::gcd(s, *j - *i);
                    }
                }
                return s;
            }
        } s_and {}, s_or {}, s_xor {};

        for (T i = x.low; ; i += x.step)
        {
            for (T j = y.low; ; j += y.step)
            {
                T r;

                r = i & j;
                s_and.add(r);
                if (r < b_and.low)
                {
                    b_and.low = r;
                }
                if (r > b_and.high)
                {
                    b_and.high = r;
                }
                if (mod(r, c_and.step) != r_and.rem)
                {
                    r_and.ok = false;
                }

                r = i | j;
                s_or.add(r);
                if (r < b_or.low)
                {
                    b_or.low = r;
                }
                if (r > b_or.high)
                {
                    b_or.high = r;
                }
                if (mod(r, c_or.step) != r_or.rem)
                {
                    r_or.ok = false;
                }

                r = i ^ j;
                s_xor.add(r);
                if (r < b_xor.low)
                {
                    b_xor.low = r;
                }
                if (r > b_xor.high)
                {
                    b_xor.high = r;
                }
                if (mod(r, c_xor.step) != r_xor.rem)
                {
                    r_xor.ok = false;
                }

                if (j == y.high) break;
            }

            if (i == x.high) break;
        }

        if constexpr (BRUTE_STEP)
        {
            b_and.step = s_and.get_step();
            b_or.step  = s_or.get_step();
            b_xor.step = s_xor.get_step();
        }
        else
        {
            b_and.step = c_and.step;
            b_or.step  = c_or.step;
            b_xor.step = c_xor.step;
        }

        bool ok = (c_and >= b_and) && r_and.ok && (c_or >= b_or) && r_or.ok && (c_xor >= b_xor) && r_xor.ok;

        if (!ok || trace)
        {
            auto ok = [] (Interval<T> const & a, Interval<T> const & b, bool r)
            {
                return
                (
                    r && (a == b) ? "OK "s
                    // :   r && (a >= b) ? std::format("OVER ({} / {} / {}) ", distance(a.low, a.high), distance(b.low, b.high), distance(a.low, a.high) - distance(b.low, b.high))
                    :   r && (a >= b) ? "OVER ("s + std::to_string(distance(a.low, a.high) - distance(b.low, b.high)) + ") "s
                :   (a == b)      ? "STEP "s
                :   (a >= b)      ? "STEP+OVER "s
                :                   "KO "s
                );
            };
            std::cout << ok(c_not_x, b_not_x, true) << "not x : " << c_not_x << " : " << b_not_x << std::endl;
            std::cout << ok(c_not_y, b_not_y, true) << "not y : " << c_not_y << " : " << b_not_y << std::endl;
            std::cout << ok(c_and, b_and, r_and.ok) << "and x y : " << c_and << " : " << b_and << std::endl;
            std::cout << ok(c_or , b_or , r_or.ok ) << "or x y : " << c_or  << " : " << b_or  << std::endl;
            std::cout << ok(c_xor, b_xor, r_xor.ok) << "xor x y : " << c_xor  << " : " << b_xor  << std::endl;
        }

        return ok;
    }

    template <typename T>
    void test(char const ** args, bool step)
    {
        Interval<T> i, j;

        try
        {
            char const ** a = args;
            i.low  = parse<T>(*a++);
            i.high = parse<T>(*a++);            
            i.step = step ? parse<T>(*a++) : 1;
            j.low  = parse<T>(*a++);
            j.high = parse<T>(*a++);
            j.step = step ? parse<T>(*a++) : 1;
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
    void test_random()
    {
        Rand rand {};

        while (true)
        {
            using UT = std::make_unsigned_t<T>;

            T a_low = rand(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());

            UT a_dist = distance(a_low, std::numeric_limits<T>::max());

            UT a_step;
            if (STEP_MODE == S_PO2)
            {
                a_step = UT(1ULL << rand(0, int((sizeof (T) * CHAR_BIT) - 1)));
            }
            else if (STEP_MODE == S_1)
            {
                a_step = UT(1);
            }
            else
            {
                a_step = rand(UT(1), a_dist);
            }
            if constexpr (std::is_signed_v<T>)
            {
                if (T(a_step) < 0) continue;
            }

            UT a_size = UT(a_dist / a_step);
            if constexpr (sizeof (UT) > 2)
            {
                if (a_size > INTERVAL_MAX) {a_size = INTERVAL_MAX;}
            }
            a_size = rand(UT(0), a_size);
            if (a_size == UT(0))
            {
                a_step = UT(1);
            }

            T a_high = T(a_low + a_size * a_step);

            T b_low = rand(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());

            UT b_dist = distance(b_low, std::numeric_limits<T>::max());

            UT b_step;
            if (STEP_MODE == S_PO2)
            {
                b_step = UT(1ULL << rand(0, int((sizeof (T) * CHAR_BIT) - 1)));
            }
            else if (STEP_MODE == S_1)
            {
                b_step = UT(1);
            }
            else
            {
                b_step = rand(UT(1), b_dist);
            }
            if constexpr (std::is_signed_v<T>)
            {
                if (T(b_step) < 0) continue;
            }

            UT b_size = UT(b_dist / b_step);
            if constexpr (sizeof (UT) > 2)
            {
                if (b_size > INTERVAL_MAX) {b_size = INTERVAL_MAX;}
            }
            b_size = rand(UT(0), b_size);
            if (b_size == UT(0))
            {
                b_step = UT(1);
            }

            T b_high = T(b_low + b_size * b_step);

            std::cout << "DBG A : " << +a_low << " / " << +a_high << " / " << +a_step << " / " << +T(a_step) << std::endl;
            std::cout << "DBG B : " << +b_low << " / " << +b_high << " / " << +b_step << " / " << +T(b_step) << std::endl;
            if (!test_interval(Interval<T> {a_low, a_high, T(a_step)}, Interval<T> {b_low, b_high, T(b_step)}, true))
            {
                break;
            }
#ifdef __TRUSTINSOFT_ANALYZER__
            break;
#endif
        }
    }
}

template <typename T>
struct Test
{
    using Type = T;
    using UType = std::make_unsigned_t<T>;

    Type s;
    UType u;

    // template <typename X>
    // Test(X s, std::make_unsigned_t<X> u) : s(Type(s)), u(UType(u)) {}

    template <typename X, typename UX>
    Test(X s, UX u) : s(Type(s)), u(UType(u))
    {
        // assert(s >= std::numeric_limits<Type>::min());
        // assert(s <= std::numeric_limits<Type>::max());
        if constexpr (std::is_signed_v<T>)
        {
            assert(u >= UX(0));
        }
    }
};

int usage(char const * file, int e = -1)
{
    std::cerr << "usage: " << file << "[-h] [-d] [-s1] [-s2] [-sa] type [x.low x.high [x_step] y.low y.high [y_step]]\n";
    return -e;
}

int main(int argc, char const * argv[])
{
    //// Test<int> i {-3.5, 10LL}; // ko std::make_unsigned_t<double>, ok for 2nd constructor
    //{Test<int> i {-3, 10LL};}
    //{Test<int> i {-3, 10U};}

    //assert(round_up(0, 3, 1) == 1);
    //assert(round_up(1, 3, 1) == 1);
    //assert(round_up(2, 3, 1) == 4);

    //assert(round_up(-3, 3, 1) == -2);
    //assert(round_up(-2, 3, 1) == -2);
    //assert(round_up(-1, 3, 1) == 1);

    //assert(round_down(6, 3, 1) == 4);
    //assert(round_down(7, 3, 1) == 7);
    //assert(round_down(8, 3, 1) == 7);

    //assert(round_down(-1, 3, 1) == -2);
    //assert(round_down(-2, 3, 1) == -2);
    //assert(round_down(-3, 3, 1) == -5);

    // skip filename
    char const ** args = &argv[1];
    
    for (bool done = false; !done && (argc > 0); ++args, --argc)
    {
        if (*args == "-h"s)
        {
            DBG_HEX = true;
        }
        else if (*args == "-d"s)
        {
            DBG_HEX = false;
        }
        else if (*args == "-s1"s)
        {
            STEP_MODE = S_1;
        }
        else if (*args == "-s2"s)
        {
            STEP_MODE = S_PO2;
        }
        else if (*args == "-sa"s)
        {
            STEP_MODE = S_ANY;
        }
        else if (*args == "--"s)
        {
            done = true;
        }
        else if (*args[0] == '-')
        {
            std::cerr << "invalid option '" << *args << "'\n";
            return usage(argv[0]);
        }
        else break;
    }

    // keep static analysis happy
    if (*args == nullptr)
    {
        return usage(argv[0]);
    }

    if (argc == 2)
    {
        static std::map<std::string, std::function<void (void)>> const tfs =
        {
            {"s8" , test_random<int8_t>},
            {"u8" , test_random<uint8_t>},
            {"s16", test_random<int16_t>},
            {"s32", test_random<int32_t>},
            {"s64", test_random<int64_t>},
            {"u16", test_random<uint16_t>},
            {"u32", test_random<uint32_t>},
            {"u64", test_random<uint64_t>},
            {"s"  , test_random<int>},
            {"u"  , test_random<unsigned int>}
        };

        if (auto f = tfs.find(args[0]); f != tfs.end())
        {
            f->second();
        }
        else
        {
            std::cerr << "invalid type '" << argv[0] << "'\n";
            return usage(argv[0]);
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

        if (auto f = tfs.find(args[0]); f != tfs.end())
        {
            f->second(args + 1, (argc == 8));
        }
        else
        {
            std::cerr << "invalid type '" << argv[1] << "'\n";
            return usage(argv[0]);
        }
    }
    else
    {
        return usage(argv[0]);
    }

    return 0;
}
