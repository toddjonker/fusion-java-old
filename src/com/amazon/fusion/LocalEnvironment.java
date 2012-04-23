// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Collection;

/**
 *
 */
final class LocalEnvironment
    implements Environment
{
    private final Environment   myEnclosure;
    private final String[]      myNames;
    private final FusionValue[] myValues;


    LocalEnvironment(Environment enclosure,
                     String[] names,
                     FusionValue[] values)
    {
        assert names.length == values.length;
        myEnclosure = enclosure;
        myNames = names;
        myValues = values;
    }

    @Override
    public FusionValue lookup(String name)
    {
        final int paramCount = myNames.length;
        for (int i = 0; i < paramCount; i++)
        {
            if (name.equals(myNames[i]))
            {
                return myValues[i];
            }
        }

        return myEnclosure.lookup(name);
    }

    @Override
    public void collectNames(Collection<String> names)
    {
        for (String name : myNames)
        {
            names.add(name);
        }
        myEnclosure.collectNames(names);
    }
}
