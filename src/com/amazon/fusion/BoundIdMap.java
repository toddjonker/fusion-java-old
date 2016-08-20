// Copyright (c) 2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A map that uses bound identifiers as keys.
 * <p>
 * Two identifiers ({@link SyntaxSymbol}s) are {@code bound_identifier_equal}
 * if the have the same (original) binding and the same marks.
 * <p>
 * This class is not thread-safe!
 *
 * @param <V> the type of mapped values.
 */
final class BoundIdMap<V>
{
    /**
     * The minimal data needed to track a bound identifier.
     */
    private static final class BoundIdentifier
    {
        /**
         * An original binding, so there's meaningful hashCode and equals.
         */
        private final Binding       myBinding;
        private final Set<MarkWrap> myMarks;

        // TODO Compress the mark-sets, there's going to be lots of
        //  empty and small sets.

        BoundIdentifier(Binding binding, Set<MarkWrap> marks)
        {
            myBinding = binding.target(); // Get the original binding.
            myMarks   = marks;
        }

        BoundIdentifier(SyntaxSymbol identifier)
        {
            this(identifier.resolve(),
                 identifier.computeMarks());
        }

        @Override
        public boolean equals(Object obj)
        {
            if (this == obj) return true;
            BoundIdentifier other = (BoundIdentifier) obj;
            return myBinding.sameTarget(other.myBinding)
                && myMarks.equals(other.myMarks);
        }

        // TODO implement Binding.hashCode for local bindings
        //    so this class can be used for local environments too.

        @Override
        public int hashCode()
        {
            final int prime = 31;
            int result = 1;
            result =
                prime * result
                    + ((myBinding == null) ? 0 : myBinding.hashCode());
            result =
                prime * result
                    + ((myMarks == null) ? 0 : myMarks.hashCode());
            return result;
        }
    }

    private final Map<BoundIdentifier, V> myMap = new HashMap<>();


    /**
     * @return the previous value associated with the identifier, or null
     *   if there was no mapping for it.
     */
    final V put(SyntaxSymbol identifier, V binding)
    {
        BoundIdentifier key = new BoundIdentifier(identifier);
        return myMap.put(key, binding);
    }

    final V get(SyntaxSymbol identifier)
    {
        return myMap.get(new BoundIdentifier(identifier));
    }

    final V get(Binding binding, Set<MarkWrap> marks)
    {
        return myMap.get(new BoundIdentifier(binding, marks));
    }

    final Collection<V> values()
    {
        return myMap.values();
    }
}
