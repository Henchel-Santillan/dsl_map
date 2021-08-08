#ifndef DS_MAP_MAP_H
#define DS_MAP_MAP_H


#include <algorithm>
#include <functional>
#include <memory_resource>

#include "internal/map_iterator.h"
#include "internal/policy.h"


namespace dsl::map
{
    template <class Key,
              class V,
              class Hash = std::hash<Key>,
              class KeyEqual = std::equal_to<Key>,
              class Allocator = std::polymorphic_allocator<std::byte>,
              class GrowthPolicy = internal::growth_policy
              >
    class map
    {

    };  // class map

}   // namespace dsl::map



#endif //DS_MAP_MAP_H
