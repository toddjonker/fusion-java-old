// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWrite;
import java.io.IOException;
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
        super(message != null ? message : "Module not found");
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
    public void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append(getBaseMessage());

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
                safeWrite(eval, out, loc);
            }
        }
    }
}
