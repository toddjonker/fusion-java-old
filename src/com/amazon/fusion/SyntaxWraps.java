// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

final class SyntaxWraps
{
    /** Not null. */
    private final SyntaxWrap[] myWraps;

    static SyntaxWraps make(SyntaxWrap initialWrap)
    {
        return new SyntaxWraps(initialWrap);
    }

    static SyntaxWraps make(SyntaxWrap... initialWraps)
    {
        return new SyntaxWraps(initialWraps);
    }

    private SyntaxWraps(SyntaxWrap initialWrap)
    {
        myWraps = new SyntaxWrap[] { initialWrap };
    }

    private SyntaxWraps(SyntaxWrap[] wraps)
    {
        myWraps = wraps;
    }


    SyntaxWraps addWrap(SyntaxWrap wrap)
    {
        int suffixLen =  myWraps.length;
        int len = 1 + suffixLen;

        SyntaxWrap[] combined = new SyntaxWrap[len];
        combined[0] = wrap;
        System.arraycopy(myWraps, 0, combined, 1, suffixLen);

        return new SyntaxWraps(combined);
    }


    /**
     * Prepends a sequence of wraps onto our existing ones.
     * It is assumed that the given list will not be modified later and can
     * therefore be shared.
     */
    SyntaxWraps addWraps(SyntaxWraps wraps)
    {
        // TODO this should use a linked-list to avoid copies
        int prefixLen = wraps.myWraps.length;
        int suffixLen =  this.myWraps.length;
        int len = prefixLen + suffixLen;

        SyntaxWrap[] combined = new SyntaxWrap[len];
        System.arraycopy(wraps.myWraps, 0, combined, 0, prefixLen);
        System.arraycopy(myWraps, 0, combined, prefixLen, suffixLen);

        return new SyntaxWraps(combined);
    }


    /**
     * @return not null.
     */
    public Set<MarkWrap> computeMarks()
    {
        Set<MarkWrap> marks = null;

        for (SyntaxWrap wrap : myWraps)
        {
            if (wrap instanceof MarkWrap)
            {
                MarkWrap mark = (MarkWrap) wrap;

                if (marks == null)
                {
                    marks = new HashSet<>();
                    marks.add(mark);
                }
                else if (! marks.add(mark))
                {
                    marks.remove(mark);
                }
            }
        }

        if (marks == null) marks = Collections.emptySet();
        return marks;
    }


    final boolean hasMarks(Evaluator eval)
    {
        // We have to walk all wraps to match up cancelling pairs of marks.
        return ! computeMarks().isEmpty();
    }


    /**
     * @return null is equivalent to a {@link FreeBinding}, and either may be
     * returned.
     */
    Binding resolve(BaseSymbol name)
    {
        if (myWraps.length == 0) return null;

        Iterator<SyntaxWrap> i = Arrays.asList(myWraps).iterator();

        SyntaxWrap wrap = i.next();
        Set<MarkWrap> marks = new HashSet<>();
        return wrap.resolve(name, i, marks);
    }

    /**
     * @return null is equivalent to a {@link FreeBinding}, and either may be
     * returned.
     */
    Binding resolveTop(BaseSymbol name)
    {
        if (myWraps.length == 0) return null;

        Iterator<SyntaxWrap> i = Arrays.asList(myWraps).iterator();

        SyntaxWrap wrap = i.next();
        Set<MarkWrap> marks = new HashSet<>();
        return wrap.resolveTop(name, i, marks);
    }
}
