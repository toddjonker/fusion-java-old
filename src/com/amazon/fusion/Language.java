// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.util.Iterator;

/**
 *
 */
public final class Language
{
    /**
     * Thown to force the exit of an evaluation.
     */
    @SuppressWarnings("serial")
    public static final class ExitException
        extends FusionException
    {
        ExitException() { super("Exit requested"); }
    }


    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final Evaluator myEvaluator = new Evaluator(mySystem);
    private final Environment myEnvironment = myEvaluator.newBaseNamespace();


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     */
    public FusionValue eval(String source)
        throws ExitException, FusionException
    {
        Iterator<IonValue> i = mySystem.iterate(source);
        return eval(i);
    }


    /**
     * @param source must not be null.
     * @return not null, but perhaps {@link FusionValue#UNDEF}.
     *
     * @throws ExitException
     */
    public FusionValue eval(Iterator<IonValue> source)
        throws ExitException, FusionException
    {
        FusionValue result = UNDEF;

        while (source.hasNext())
        {
            IonValue sourceExpr = source.next();
            result = myEvaluator.eval(myEnvironment, sourceExpr);
        }

        return result;
    }
}
