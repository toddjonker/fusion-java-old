// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

/**
 *
 */
final class ModuleInstance
    extends NamedValue
{
    private final Evaluator myEvaluator;
    private final Environment myEnvironment;

    ModuleInstance(Evaluator eval, Environment env)
    {
        myEvaluator = eval;
        myEnvironment = env;
    }


    @Override
    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous module");
        }
        else
        {
            out.append("module ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    void useIn(CoreEnvironment env)
    {
        Collection<String> names = new ArrayList<String>();
        myEnvironment.collectNames(names);
        for (String name : names)
        {
            FusionValue value = myEnvironment.lookup(name);
            env.bind(name, value);
        }
    }
}
