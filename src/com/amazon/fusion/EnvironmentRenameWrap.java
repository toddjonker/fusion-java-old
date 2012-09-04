// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Environment.Binding;
import java.util.Iterator;

/**
 * Syntax wrap that adds all bindings from a specific namespace.
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
    Binding resolve(SyntaxSymbol identifier, Iterator<SyntaxWrap> moreWraps)
    {
        Binding b;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(identifier, moreWraps);
        }
        else
        {
            b = new FreeBinding(identifier.stringValue());
        }

        Binding subst = myEnvironment.substitute(b);
        return subst;
    }


    @Override
    public String toString()
    {
        return "/* Environment renames */";
    }
}
