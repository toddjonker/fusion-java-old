// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap holding marks applied by the macro expander.
 */
class MarkWrap
    extends SyntaxWrap
{
    private final int myMark;

    MarkWrap(int mark)
    {
        myMark = mark;
    }

    int getMark()
    {
        return myMark;
    }


    @Override
    Binding resolve(String identifier,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding b;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(identifier, moreWraps, returnMarks);
        }
        else
        {
            b = new FreeBinding(identifier);
        }

        if (! returnMarks.add(myMark))
        {
            returnMarks.remove(myMark);
        }

        return b;
    }


    @Override
    public String toString()
    {
        return "{{mark " + myMark + "}}";
    }
}
