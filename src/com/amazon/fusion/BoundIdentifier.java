// Copyright (c) 2016-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Set;

/**
 * The minimal data needed to track a bound identifier.
 */
final class BoundIdentifier
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

    Binding getBinding()
    {
        return myBinding;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj) return true;
        BoundIdentifier other = (BoundIdentifier) obj;
        return myBinding.sameTarget(other.myBinding)
            && myMarks.equals(other.myMarks);
    }

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
