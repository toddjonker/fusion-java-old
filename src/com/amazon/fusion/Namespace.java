// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 */
class Namespace
    implements Environment
{
    private final Map<String,FusionValue> myBindings =
        new HashMap<String,FusionValue>();

    @Override
    public Namespace namespace()
    {
        return this;
    }

    public void bind(String name, FusionValue value)
    {
        value.inferName(name);
        myBindings.put(name, value);
    }


    void use(ModuleInstance env)
    {
        Namespace moduleNamespace = env.getNamespace();
        Collection<String> names = new ArrayList<String>();
        moduleNamespace.collectNames(names);
        for (String name : names)
        {
            FusionValue value = moduleNamespace.lookup(name);
            myBindings.put(name, value);
        }
    }

    @Override
    public FusionValue lookup(String name)
    {
        return myBindings.get(name);
    }

    @Override
    public void collectNames(Collection<String> names)
    {
        names.addAll(myBindings.keySet());
    }
}
