-- Not a Functor

-- The Ord constraint prevents a lawful SortedList Functor instance. A regular list fmap can make the list unsorted, and an fmap that sorts the list could change the structure.
