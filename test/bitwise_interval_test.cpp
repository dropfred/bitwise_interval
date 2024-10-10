#include <bitwise_interval.h>

#include <iostream>
#include <cstdint>

namespace
{
    template <typename T>
    std::ostream & operator << (std::ostream & os, Interval<T> const & i)
    {
        os << std::hex;

        if (i.is_singleton())
        {
            os << '{' << +i.low << "}";
        }
        else
        {
            os << '[' << +i.low << "; " << +i.high << "]";
            if (i.step > 1)
            {
                os << "/" << +i.step;
            }
        }
        return os;
    }
}

void test_bi()
{
    using UInt32 = Interval<std::uint32_t>;

    UInt32 rd  {0, 0xC0, 1U};
    UInt32 rs0 {0, 0xC0, 1U};
    UInt32 rs1 {0, 0xC0, 1U};

    UInt32 instr = (rd << 16) | (rs0 << 8) | (rs1 << 0);

    // rd  = (instr >> 16) & 0xFF;
    // rs0 = (instr >>  8) & 0xFF;
    // rs1 = (instr >>  0) & 0xFF;
    // std::cout << "rd: " << rd << '\n';
}

int main()
{
    test_bi();

    return 0;
}