// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionCollection.isCollection;
import static com.amazon.fusion.FusionCollection.unsafeCollectionAnnotate;
import com.amazon.ion.IonValue;


final class AnnotateProc
    extends Procedure
{
    AnnotateProc()
    {
        super("... doesn't mutate the value ...",
              "value", "symbol", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        int arity = args.length;

        String[] annotations = new String[arity - 1];
        for (int i = 0; i < arity - 1; i++)
        {
            String a = checkTextArg(i+1, args);
            if (a.isEmpty())
            {
                throw argFailure("valid annotation", i+1, args);
            }
            annotations[i] = a;
        }

        Object target = args[0];
        if (isCollection(eval, target))
        {
            return unsafeCollectionAnnotate(eval, target, annotations);
        }

        IonValue value = castToIonValueMaybe(target);
        if (value == null)
        {
            throw argFailure("annotatable type", 0, args);
        }

        value = value.clone();
        value.setTypeAnnotations(annotations);
        return value;
    }
}
