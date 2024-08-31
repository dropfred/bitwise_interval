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
#include <cmath>
#include <set>
#include <numeric>
#include <filesystem>
// #include <format>

#include <variant>
#include <initializer_list>

#include  <cstdlib> // rand

using namespace std::string_literals;

#define DEV_REPLAY_RAND

#ifndef INTERVAL_MAX
// #define INTERVAL_MAX 0x10000U
#define INTERVAL_MAX 0x1000U
#endif

#define BRUTE_STEP_MAX 1

namespace
{
    struct Cfg
    {
        enum
        {
            S_1,
            S_PO2,
            S_ANY

        };
        int step = S_ANY;
        bool hex = false;
        std::size_t num = 0;
    } cfg;

    template <typename T>
    bool operator == (Interval<T> const & a, Interval<T> const & b)
    {
        return ((a.low == b.low) && (a.high == b.high) && (a.step == b.step));
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
            if (cfg.hex)
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
            if (cfg.hex)
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
        if (i.is_singleton())
        {
            os << '{' << Dbg {i.low} << "}";
        }
        else
        {
            os << '[' << Dbg {i.low} << ", " << Dbg  {i.high} << "]";
            if (i.step > 1)
            {
                os << "/" << Dbg {+i.step};
            }
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
             : (b <  0) ? UT(UT(b) - UT(a))
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
    class Rand
    {
        std::random_device rd {};

    public:

        template <typename T>
        T operator () (T a, T b)
        {
            using D = std::uniform_int_distribution<std::conditional_t<(sizeof (T) > 1), T, short int>>;
            return T((D {a, b})(rd));
        }
    };
#endif

    enum
    {
        S_OK,
        S_OVER,
        S_KO
    };

    template <typename T>
    struct Score
    {
        int type;

        using UT = std::make_unsigned_t<T>;

        struct
        {
            UT distance;
            UT step;
        } over;
    };

    template <typename T>
    Score<T> score(Interval<T> const & a, Interval<T> const & b)
    {
        Score<T> s {S_OVER};

        if (a == b)
        {
            s.type = S_OK;
        }
        else if (a >= b)
        {
            s.type = S_OVER;
            s.over.distance = distance(distance(b.low, b.high), distance(a.low, a.high));
            s.over.step = b.step / a.step;
        }
        else
        {
            s.type = S_KO;
        }

        return s;
    }

    Rand rand {};

    template <typename T>
    bool test_interval(Interval<T> const x, Interval<T> const y, bool trace = true)
    {
        using UT = Interval<T>::UType;

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
        // return ((c_not_x == b_not_x) && (c_not_y == b_not_y));

        auto c_and = and_interval(x, y);
        auto c_or  = or_interval(x, y);
        auto c_xor = xor_interval(x, y);

        struct
        {
            UT rem;
            bool ok = true;
        } r_and {umod(c_and.low, c_and.step)}, r_or {umod(c_or.low, c_or.step)}, r_xor {umod(c_xor.low, c_xor.step)};

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

            UT get_step() const
            {
                UT s = 1;
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
                if (umod(r, c_and.step) != r_and.rem)
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
                if (umod(r, c_or.step) != r_or.rem)
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
                if (umod(r, c_xor.step) != r_xor.rem)
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

        bool ok = (c_not_x >= b_not_x) && (c_not_y >= b_not_y) && (c_and >= b_and) && r_and.ok && (c_or >= b_or) && r_or.ok && (c_xor >= b_xor) && r_xor.ok;

        if (!ok || trace)
        {
            auto ok = [] (Interval<T> const & a, Interval<T> const & b, bool r)
            {
                auto [type, over] = score(a, b);
                return
                (
                    r && (type == S_OK)   ? "OK "s :
                    // r && (score == S_OVER) ? std::format("OVER ({}) ", over.distance, over.step) :
                    r && (type == S_OVER) ? "OVER ("s + std::to_string(over.distance) + " / " + std::to_string(over.step) + ") " :
                                            "KO "s
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
    Interval<T> make_random_interval()
    {
        using UT = std::make_unsigned_t<T>;

        T low = rand(std::numeric_limits<T>::min(), std::numeric_limits<T>::max());

        UT dist = distance(low, std::numeric_limits<T>::max());

        UT step;
        if (cfg.step == Cfg::S_PO2)
        {
            step = UT(1ULL << rand(0, int((sizeof (T) * CHAR_BIT) - 1)));
        }
        else if (cfg.step == Cfg::S_1)
        {
            step = UT(1);
        }
        else
        {
            step = rand(UT(1), dist);
        }

        UT size = UT(dist / step);
        if constexpr (sizeof (UT) > 2)
        {
            if (size > INTERVAL_MAX) {size = INTERVAL_MAX;}
        }
        size = rand(UT(0), size);
        if (size == UT(0))
        {
            step = UT(1);
        }

        T high = T(low + size * step);

        return {low, high, step};
    }

    template <typename T>
    void test_random()
    {
        size_t ns = cfg.num;

        while (true)
        {
            if (!test_interval(make_random_interval<T>(), make_random_interval<T>(), true))
            {
                break;
            }
            if ((ns != 0) && (--ns == 0))
            {
                break;
            }
#ifdef __TRUSTINSOFT_ANALYZER__
            break;
#endif
        }
    }

    [[noreturn]] int usage(char const * file, int e = -1)
    {
        auto p = (std::filesystem::path {file}).filename().string();
        std::cerr << "usage: " << p << " -h\n";
        std::cerr << "     : " << p << " [-d] [-x] [-s1] [-s2] [-sa] [-n=<num>] <type>\n";
        std::cerr << "     : " << p << " [-d] [-x] <type> <x low> <x high> <y low> <y high>\n";
        std::cerr << "     : " << p << " [-d] [-x] <type> <x low> <x high> <x step> <y low> <y high> <y step>\n";
        exit(e);
    }

    class CmdLine
    {
        std::map<std::string, std::function<bool ()>> ks;
        std::map<std::string, std::function<bool (std::string const &)>> kvs;

    public :

        CmdLine() : ks {}, kvs {} {}

        CmdLine(CmdLine const &) = default;
        CmdLine(CmdLine &&) = default;

        CmdLine & operator = (CmdLine const &) = default;

        struct NF
        {
            std::string name;
            std::variant
                <
                std::function<void ()>,
                std::function<bool ()>,
                std::function<void (std::string const &)>,
                std::function<bool (std::string const &)>
                > fun;
        };

        CmdLine(std::initializer_list<NF> nfs) : ks {}, kvs {}
        {

            for (auto const & [n, f] : nfs)
            {
                std::visit([&] (auto && f) {add(n, f);}, f);
            }
        }

        CmdLine & add(std::string const & k, std::function<bool ()> f)
        {
            ks[k] = f;
            return *this;
        }

        CmdLine & add(std::string const & k, std::function<void ()> f)
        {
            ks[k] = [f] () {f(); return true;};
            return *this;
        }

        CmdLine & add(std::string const & k, std::function<bool (std::string const &)> f)
        {
            kvs[k] = f;
            return *this;
        }

        CmdLine & add(std::string const & k, std::function<void (std::string const &)> f)
        {
            kvs[k] = [f] (std::string const & v) {f(v); return true;};
            return *this;
        }

        int operator () (char const * argv[], bool skip_first = true) const
        {
            auto unk = [] (std::string const & s)
                {
                    std::cerr << "unknown option '" << s  << "'\n";
                    return -1;
                };

            auto error = [] (std::string const & s)
                {
                    std::cerr << "error '" << s  << "'\n";
                    return -2;
                };

            int argc = skip_first ? 1 : 0;

            for (char const ** c = &argv[argc]; (*c != nullptr) && (**c == '-'); ++c)
            {
                ++argc;

                std::string s {*c};

                if (s == "--"s) break;

                auto e = s.find('=');
                if (e != std::string::npos)
                {
                    auto k = s.substr(0, e);
                    auto v = s.substr(e + 1);
                    auto f = kvs.find(k);
                    if (f == kvs.end())
                    {
                        return unk(k);
                    }
                    if (!f->second(v))
                    {
                        return error(s);
                    }
                }
                else
                {
                    auto f = ks.find(s);
                    if (f == ks.end())
                    {
                        return unk(s);
                    }
                    if (!f->second())
                    {
                        return error(s);
                    }
                }
            }
            return argc;
        }
    };
}
int main(int argc, char const * argv[])
{
    try
    {
        int args = CmdLine
        {
            {"-h" , [argv] () {usage(argv[0], 0);}},
            {"-d" , [] () {cfg.hex = false;}},
            {"-x" , [] () {cfg.hex = true;}},
            {"-s1", [] () {cfg.step = Cfg::S_1;}},
            {"-s2", [] () {cfg.step = Cfg::S_PO2;}},
            {"-sa", [] () {cfg.step = Cfg::S_ANY;}},
            //{"-n" , [] (std::string const & v) {try {cfg.num = parse<std::size_t>(v);} catch (...) {return false;} return true;}}
            {"-n" , [] (std::string const & v) {cfg.num = parse<std::size_t>(v);}}
        } (argv);

        if (args < 0)
        {
            usage(argv[0]);
        }

        argc -= args;

        // keep static analysis happy
        if (argv[args] == nullptr)
        {
            usage(argv[0]);
            return -1; // [[noreturn]] attribute ignored by vsc intellisense
        }

        if (argc == 1)
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

            if (auto f = tfs.find(argv[args]); f != tfs.end())
            {
                f->second();
            }
            else
            {
                std::cerr << "invalid type '" << argv[args] << "'\n";
                usage(argv[0]);
            }
        }
        else if ((argc == 5) || (argc == 7))
        {
            std::function<void (char const **, bool)> dbg = test<int8_t>;
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

            if (auto f = tfs.find(argv[args]); f != tfs.end())
            {
                f->second(&argv[args + 1], (argc == 7));
            }
            else
            {
                std::cerr << "invalid type '" << argv[args] << "'\n";
                usage(argv[0]);
            }
        }
        else
        {
            usage(argv[0]);
        }
    }
    catch (std::exception const &)
    {
        usage(argv[0]);
    }    

    return 0;
}
