#ifndef DS_MAP_POLICY_H
#define DS_MAP_POLICY_H


#include <cstddef>
#include <limits>


namespace dsl::map::details
{
    // Power of two growth policy
    struct growth_policy
    {
        static constexpr std::size_t compute_index(const std::size_t hash, const std::size_t capacity)
        { return hash & (capacity - 1); }

        static constexpr std::size_t compute_closest_capacity(const std::size_t min_capacity)
        {
            constexpr auto max_capacity { std::size_t{1} << (std::numeric_limits<std::size_t>::digits - 1) };
            if (min_capacity > max_capacity)
                return max_capacity;
            min_capacity--;
            for (auto i = 1; i < std::numeric_limits<std::size_t>::digits; i *= 2)
                min_capacity |= min_capacity >> i;
            return ++min_capacity;
        }

        static constexpr std::size_t minimum_capacity() { return 8u; }

    };  // struct growth_policy

}   // namespace dsl::map::internal


#endif //DS_MAP_POLICY_H
