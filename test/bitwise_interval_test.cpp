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
#include <set>
#include <numeric>
#include <filesystem>

#include <utility>
#include <variant>
#include <initializer_list>

#include <cstdlib>
#include <cassert>
#include <cmath>
#include <climits>

using namespace std::string_literals;

#define DEV_REPLAY_RAND

#ifndef INTERVAL_MAX
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
    struct Dbg
    {
        T value;
        bool padding;
        Dbg(T value, bool padding = true) : value(value), padding(padding) {}
    };

    template <typename T>
    std::ostream & operator << (std::ostream & os, Dbg<T> const & v)
    {
        if constexpr (sizeof (T) == 1)
        {
            if (cfg.hex)
            {
                os << std::bitset<CHAR_BIT>((unsigned long long)(v.value)) << " (" << std::setw(2) << std::setfill('0') << std::hex << std::uppercase << (v.value & 0xFF) << ")";
            }
            else
            {
                os << std::bitset<CHAR_BIT>((unsigned long long)(v.value)) << " (" << std::setw(0) << std::dec << +v.value << ")";
            }
        }
        else
        {
            if (cfg.hex)
            {
                if (v.padding)
                {
                    os << std::setw(2 * sizeof (T)) << std::setfill('0');
                }
                else
                {
                    os << std::setw(0);
                }
                os << std::hex << std::uppercase << v.value;
            }
            else
            {
                os << std::setw(0) << std::dec << v.value;
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
                os << "/" << Dbg {+i.step, false};
            }
        }
        return os;
    }

    template <typename T>
    T parse(std::string const & str)
    {
        bool b = (str.find("0b") == 0) || (str.find("0B") == 0);
        T v;
        if constexpr (std::is_signed_v<T>)
        {
            v = T(b ? std::stoll(str.c_str() + 2, nullptr, 2) : std::stoll(str, nullptr, 0));
        }
        else
        {
            v = T(b ? std::stoull(str.c_str() + 2, nullptr, 2) : std::stoull(str, nullptr, 0));
        }
        return v;
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
        S_KO,
        S_OK,
        S_OVER
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
        Score<T> s {S_KO};

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

        return s;
    }

    Rand rand {};

    template <typename T>
    bool test_interval(Interval<T> const x, Interval<T> const y, bool trace = true)
    {
        using UT = Interval<T>::UType;

        std::cout << "# x = " << x << "\n# y = " << y << std::endl;

        auto c_not_x = ~x;
        auto c_not_y = ~y;

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

        auto c_and_xy = x & y;
        auto c_or_xy  = x | y;
        auto c_xor_xy = x ^ y;

        Interval<T> b_and_xy, b_or_xy, b_xor_xy;

        b_and_xy.low  = b_or_xy.low  = b_xor_xy.low  = std::numeric_limits<T>::max();
        b_and_xy.high = b_or_xy.high = b_xor_xy.high = std::numeric_limits<T>::min();

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
                UT s;
                if (vs.size() > 1)
                {
                    auto j = vs.begin();
                    auto i = j++;
                    s = distance(*i, *j);
                    for (i = j++; (j != vs.end()) && (s > 1); i = j++)
                    {
                        s = std::gcd(s, distance(*i, *j));
                    }
                }
                else
                {
                    s = 0;
                }
                return s;
            }
        } s_and_xy {}, s_or_xy {}, s_xor_xy {};

        for (T i = x.low; ; i += x.step)
        {
            for (T j = y.low; ; j += y.step)
            {
                T r;

                r = i & j;
                s_and_xy.add(r);
                if (r < b_and_xy.low)
                {
                    b_and_xy.low = r;
                }
                if (r > b_and_xy.high)
                {
                    b_and_xy.high = r;
                }

                r = i | j;
                s_or_xy.add(r);
                if (r < b_or_xy.low)
                {
                    b_or_xy.low = r;
                }
                if (r > b_or_xy.high)
                {
                    b_or_xy.high = r;
                }

                r = i ^ j;
                s_xor_xy.add(r);
                if (r < b_xor_xy.low)
                {
                    b_xor_xy.low = r;
                }
                if (r > b_xor_xy.high)
                {
                    b_xor_xy.high = r;
                }

                if (j == y.high) break;
            }

            if (i == x.high) break;
        }

        if constexpr (BRUTE_STEP)
        {
            b_and_xy.step = s_and_xy.get_step();
            b_or_xy.step  = s_or_xy.get_step();
            b_xor_xy.step = s_xor_xy.get_step();
        }
        else
        {
            b_and_xy.step = c_and_xy.step;
            b_or_xy.step  = c_or_xy.step;
            b_xor_xy.step = c_xor_xy.step;
        }

        bool ok = (c_not_x >= b_not_x) && (c_not_y >= b_not_y) && (c_and_xy >= b_and_xy) && (c_or_xy >= b_or_xy) && (c_xor_xy >= b_xor_xy);

        if (!ok || trace)
        {
            auto ok = [] (Interval<T> const & a, Interval<T> const & b)
            {
                auto [type, over] = score(a, b);
                return
                (
                    (type == S_OK)   ? "OK "s :
                    (type == S_OVER) ? "OVER ("s + std::to_string(over.distance) + " / " + std::to_string(over.step) + ") " :
                                       "KO "s
                );
            };
            std::cout << ok(c_not_x , b_not_x ) << "not x   : " << c_not_x  << " : " << b_not_x  << std::endl;
            std::cout << ok(c_not_y , b_not_y ) << "not y   : " << c_not_y  << " : " << b_not_y  << std::endl;
            std::cout << ok(c_and_xy, b_and_xy) << "and x y : " << c_and_xy << " : " << b_and_xy << std::endl;
            std::cout << ok(c_or_xy , b_or_xy ) << "or x y  : " << c_or_xy  << " : " << b_or_xy  << std::endl;
            std::cout << ok(c_xor_xy, b_xor_xy) << "xor x y : " << c_xor_xy << " : " << b_xor_xy << std::endl;
        }

        return ok;
    }

    template <typename T>
    void test(char const ** args, bool step)
    {
        try
        {
            using UT = std::make_unsigned_t<T>;

            T xl, xh, yl, yh;
            UT xs, ys;

            {
                char const ** a = args;
                xl = parse<T>(*a++);
                xh = parse<T>(*a++);
                xs = step ? parse<UT>(*a++) : UT((xl == xh) ? 0 : 1);
                yl = parse<T>(*a++);
                yh = parse<T>(*a++);
                ys = step ? parse<UT>(*a++) : UT((yl == yh) ? 0 : 1);
            }

            test_interval(Interval<T> {xl, xh, xs}, Interval<T> {yl, yh, ys}, true);
        }
        catch(...)
        {
            std::cerr << "invalid interval\n";
            return;
        }
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
        if (size > INTERVAL_MAX) {size = UT(INTERVAL_MAX);}
        size = rand(UT(0), size);
        if (size == UT(0))
        {
            step = UT(0);
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
        }
    }

    class CmdLine
    {
        std::map<std::string, std::function<void ()>> ks;
        std::map<std::string, std::function<void (std::string const &)>> kvs;

    public :

        CmdLine() : ks {}, kvs {} {}

        CmdLine(CmdLine const &) = default;
        CmdLine(CmdLine &&) = default;

        CmdLine & operator = (CmdLine const &) = default;

        using Handler = std::variant
        <
            std::function<void ()>,
            std::function<void (std::string const &)>
        >;
        using Option = std::pair<std::string, Handler>;

        CmdLine(std::initializer_list<Option> os) : ks {}, kvs {}
        {
            for (auto const & [n, h] : os)
            {
                std::visit([&] (auto && h) {add(n, h);}, h);
            }
        }

        CmdLine & add(std::string const & k, std::function<void ()> f)
        {
            ks[k] = f;
            return *this;
        }

        CmdLine & add(std::string const & k, std::function<void (std::string const &)> f)
        {
            kvs[k] = f;
            return *this;
        }

        CmdLine & remove(std::string const & k)
        {
            if (auto i = ks.find(k); i != ks.end())
            {
                ks.erase(i);
            }
            else if (auto i = kvs.find(k); i != kvs.end())
            {
                kvs.erase(i);
            }
            return *this;
        }

        int operator () (char const * argv[], bool skip_first = true) const
        {
            auto unk = [] (std::string const & s)
            {
                std::cerr << "unknown option '" << s  << "'\n";
                return -1;
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
                    if (auto f = kvs.find(s.substr(0, e)); f != kvs.end())
                    {
                        f->second(s.substr(e + 1));
                    }
                    else
                    {
                        return unk(s);
                    }
                }
                else
                {
                    if (auto f = ks.find(s); f != ks.end())
                    {
                        f->second();
                    }
                    else
                    {
                        return unk(s);
                    }
                }
            }
            return argc;
        }
    };

    [[noreturn]] int usage(char const * file, int e = -1)
    {
        auto p = (std::filesystem::path {file}).filename().string();
        std::cerr << "usage: " << p << " -h\n";
        std::cerr << "     : " << p << " [-d] [-x] [-s1] [-s2] [-sa] [-n=<num>] <type>\n";
        std::cerr << "     : " << p << " [-d] [-x] <type> <x low> <x high> <y low> <y high>\n";
        std::cerr << "     : " << p << " [-d] [-x] <type> <x low> <x high> <x step> <y low> <y high> <y step>\n";
        std::exit(e);
    }
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
            {"-n" , [] (std::string const & v) {cfg.num = parse<std::size_t>(v);}}
        } (argv);

        if (args < 0)
        {
            usage(argv[0]);
        }

        char const * type = argv[args];

        // keep static analyzer happy
        if (type == nullptr)
        {
            throw;
        }

        auto invalid = [type] () {std::cerr << "invalid type '" << type << "'\n"; throw;};

        argc -= args;

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

            if (auto f = tfs.find(type); f != tfs.end())
            {
                f->second();
            }
            else
            {
                invalid();
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

            if (auto f = tfs.find(type); f != tfs.end())
            {
                f->second(&argv[args + 1], (argc == 7));
            }
            else
            {
                invalid();
            }
        }
        else
        {
            throw;
        }
    }
    catch (...)
    {
        usage(argv[0]);
    }

    return 0;
}
