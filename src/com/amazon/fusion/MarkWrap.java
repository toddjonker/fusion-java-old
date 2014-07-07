// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Syntax wrap holding marks applied by the macro expander.
 */
class MarkWrap
    extends SyntaxWrap
{
    private static final AtomicInteger ourMarkCounter = new AtomicInteger();

    private final int myMark;

    MarkWrap()
    {
        myMark = ourMarkCounter.incrementAndGet();
    }

    int getMark()
    {
        return myMark;
    }


    @Override
    Binding resolve(String name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding b = null;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(name, moreWraps, returnMarks);
        }

        if (! returnMarks.add(myMark))
        {
            returnMarks.remove(myMark);
        }

        return b;
    }


    @Override
    Iterator<SyntaxWrap> iterator()
    {
        return null;
    }


    @Override
    public String toString()
    {
        return "{{{Mark " + myMark + "}}}";
    }
}
