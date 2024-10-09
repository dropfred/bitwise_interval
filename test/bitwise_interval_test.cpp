#include <bitwise_interval.h>

#include <cstdint>

void test_bi()
{
    using UInt32 = Interval<std::uint32_t>;

    UInt32 rd {0, 192};
    UInt32 rs0 {0, 192};
    UInt32 rs1 {0, 192};

    UInt32 inst = (rd << 16) | (rs0 << 8) | (rs1 << 0);
}

int main()
{
    test_bi();

    return 0;
}