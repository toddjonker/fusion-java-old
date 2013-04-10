// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap that adds all bindings from a specific environment.
 */
class EnvironmentRenameWrap
    extends SyntaxWrap
{
    private final Environment myEnvironment;

    EnvironmentRenameWrap(Environment environment)
    {
        myEnvironment = environment;
    }

    Environment getEnvironment()
    {
        return myEnvironment;
    }

    @Override
    Binding resolve(String name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding b;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(name, moreWraps, returnMarks);
        }
        else
        {
            b = new FreeBinding(name);
        }

        Binding subst = myEnvironment.substitute(b, returnMarks);
        return subst;
    }


    @Override
    public String toString()
    {
        return "{{{Environment renames}}}";
    }
}
