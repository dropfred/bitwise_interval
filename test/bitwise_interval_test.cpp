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
#define INTERVAL_MAX 0x400U
#endif

namespace
{
    struct Cfg
    {
        enum
        {
            S_0,   // singleton
            S_1,   // no step
            S_PO2, // power of two step
            S_ANY  // any step

        };
        int step = S_1;
        bool hex = false;
        std::size_t num = 0;
        int verbose = 2;
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
            os << '[' << Dbg {i.low} << "; " << Dbg  {i.high} << "]";
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

    Rand rand {};

    template <typename T>
    bool test_interval(Interval<T> const x, Interval<T> const y)
    {
        using UT = Interval<T>::UType;

        struct Test
        {
            std::set<T> values = {};

            void add(T v) {values.insert(v);}

            Interval<T> operator () () const
            {
                assert(!values.empty());
                UT step;
                if (values.size() > 1)
                {
                    auto j = values.begin();
                    auto i = j++;
                    step = distance(*i, *j);
                    for (i = j++; (j != values.end()) && (step > 1); i = j++)
                    {
                        step = std::gcd(step, distance(*i, *j));
                    }
                }
                else
                {
                    step = 0;
                }
                return {*values.begin(), *values.rbegin(), step};
            }
        };

        struct Ko {};
        struct Ok {};
        struct Over
        {
            std::uint64_t distance;
            std::uint64_t step;
        };

        using Score = std::variant<Ko, Over, Ok>;

        auto score = [] (auto const & i, auto const & r)
        {
            Score score {};
            if      (i == r) {score = Ok {};}
            else if (i >= r) {score = Over {distance(distance(r.low, r.high), distance(i.low, i.high)), std::uint64_t(r.step / i.step)};}
            return score;
        };

        Interval<T> c_not_x  = ~x;
        Interval<T> c_not_y  = ~y;
        Interval<T> c_and_xy = x & y;
        Interval<T> c_or_xy  = x | y;
        Interval<T> c_xor_xy = x ^ y;

        Interval<T> b_not_x;
        Interval<T> b_not_y;
        Interval<T> b_and_xy;
        Interval<T> b_or_xy;
        Interval<T> b_xor_xy;

        Score s_not_x;
        Score s_not_y;
        Score s_and_xy;
        Score s_or_xy;
        Score s_xor_xy;

        {
            Test t_not_x {};

            for (T i = x.low; ; i += x.step)
            {
                t_not_x.add(T(~i));
                if (i == x.high) break;
            }
            b_not_x = t_not_x();
            s_not_x = score(c_not_x, b_not_x);
        }

        {
            Test t_not_y {};

            for (T i = y.low; ; i += y.step)
            {
                t_not_y.add(T(~i));
                if (i == y.high) break;
            }
            b_not_y = t_not_y();
            s_not_y = score(c_not_y, b_not_y);
        }

        {
            Test t_and_xy {};
            Test t_or_xy  {};
            Test t_xor_xy {};

            for (T i = x.low; ; i += x.step)
            {
                for (T j = y.low; ; j += y.step)
                {
                    t_and_xy.add(T(i & j));
                    t_or_xy .add(T(i | j));
                    t_xor_xy.add(T(i ^ j));

                    if (j == y.high) break;
                }
                if (i == x.high) break;
            }
            b_and_xy = t_and_xy();
            b_or_xy  = t_or_xy ();
            b_xor_xy = t_xor_xy();
            s_and_xy = score(c_and_xy, b_and_xy);
            s_or_xy  = score(c_or_xy , b_or_xy );
            s_xor_xy = score(c_xor_xy, b_xor_xy);
        }

        auto scores = {s_not_x, s_not_y, s_and_xy, s_or_xy, s_xor_xy};

        bool ko = std::any_of(std::begin(scores), std::end(scores), [] (auto const & s) {return std::holds_alternative<Ko>(s);});
        bool ok = std::all_of(std::begin(scores), std::end(scores), [] (auto const & s) {return std::holds_alternative<Ok>(s);});

        if ((ko && (cfg.verbose >= 0)) || (ok && (cfg.verbose >= 2)) || (!ok && cfg.verbose >= 1))
        {
            auto score = [] (Score score)
            {
                std::string s;
                if (std::holds_alternative<Ok>(score))
                {
                    s = "OK "s;
                }
                else if (std::holds_alternative<Ko>(score))
                {
                    s = "KO "s;
                }
                else
                {
                    Over const & over = std::get<Over>(score);
                    s = "OVER ("s + std::to_string(over.distance) + " / "s + std::to_string(over.step) + ") "s;
                }
                return s;
            };

            std::cout << "# x = " << x << "\n# y = " << y << '\n';
            std::cout << "not x   : " << score(s_not_x ) << c_not_x  << " : " << b_not_x  << '\n';
            std::cout << "not y   : " << score(s_not_y ) << c_not_y  << " : " << b_not_y  << '\n';
            std::cout << "and x y : " << score(s_and_xy) << c_and_xy << " : " << b_and_xy << '\n';
            std::cout << "or x y  : " << score(s_or_xy ) << c_or_xy  << " : " << b_or_xy  << '\n';
            std::cout << "xor x y : " << score(s_xor_xy) << c_xor_xy << " : " << b_xor_xy << '\n';
        }
        
        return !ko;
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
            cfg.verbose = 2;
            test_interval(Interval<T> {xl, xh, xs}, Interval<T> {yl, yh, ys});
        }
        catch (...)
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
            step = 1;
        }
        else if (cfg.step == Cfg::S_0)
        {
            step = 0;
        }
        else
        {
            step = rand(UT(1), dist);
        }

        UT size = (step > 0) ? UT(dist / step) : UT(0);
        if (size > INTERVAL_MAX) {size = UT(INTERVAL_MAX);}
        size = rand(UT(0), size);
        if (size == 0)
        {
            step = 0;
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
            if (!test_interval(make_random_interval<T>(), make_random_interval<T>()))
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
        std::cerr << "     : " << p << " [-d] [-x] [-s=<0|1|po2|any>] [-n=<num>] <type>\n";
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
            {"-v" , [argv] (std::string const & v) {cfg.verbose = parse<decltype (cfg.verbose)>(v);}},
            {"-d" , [] () {cfg.hex = false;}},
            {"-x" , [] () {cfg.hex = true;}},
            {"-s" , [] (std::string const & v) {cfg.step = (v == "0") ? Cfg::S_0 : (v == "1") ? Cfg::S_1 : (v == "po2") ? Cfg::S_PO2 : (v == "any") ? Cfg::S_ANY : throw;}},
            {"-n" , [] (std::string const & v) {cfg.num = parse<decltype (cfg.num)>(v);}}
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
