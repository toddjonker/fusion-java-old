// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Arrays;


/**
 * Indicates failure to locate a required module.
 */
@SuppressWarnings("serial")
public class ModuleNotFoundFailure
    extends FusionException
{
    private Object[] myLocations;

    ModuleNotFoundFailure(String message, Object... locations)
    {
        super(message);
        myLocations = locations;
    }

    void addContext(Object location)
    {
        int len = myLocations.length;

        // Avoid adding a duplicate entry
        if (len != 0 && location != myLocations[len - 1])
        {
            myLocations = Arrays.copyOf(myLocations, len + 1);
            myLocations[len] = location;
        }
    }

    @Override
    public String getMessage()
    {
        StringBuilder out = new StringBuilder();
        out.append(super.getMessage());

        if (myLocations.length != 0)
        {
            for (Object loc : myLocations)
            {
                out.append("\n  at ");
                if (loc instanceof SyntaxValue)
                {
                    SourceLocation sloc = ((SyntaxValue) loc).getLocation();
                    if (sloc != null)
                    {
                        sloc.display(out);
                        out.append("\n    ");
                    }
                }
                FusionValue.write(out, loc);
            }
        }
        return out.toString();
    }
}
