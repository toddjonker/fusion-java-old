// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.isBool;
import static com.amazon.fusion.FusionBool.unsafeBoolAnnotate;
import static com.amazon.fusion.FusionCollection.isCollection;
import static com.amazon.fusion.FusionCollection.unsafeCollectionAnnotate;
import static com.amazon.fusion.FusionNull.isNullNull;
import static com.amazon.fusion.FusionNull.makeNullNull;
import static com.amazon.fusion.FusionNumber.isDecimal;
import static com.amazon.fusion.FusionNumber.isFloat;
import static com.amazon.fusion.FusionNumber.isInt;
import static com.amazon.fusion.FusionNumber.unsafeDecimalAnnotate;
import static com.amazon.fusion.FusionNumber.unsafeFloatAnnotate;
import static com.amazon.fusion.FusionNumber.unsafeIntAnnotate;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.unsafeStringAnnotate;
import static com.amazon.fusion.FusionSymbol.isSymbol;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolAnnotate;
import static com.amazon.fusion.FusionText.checkNonEmptyTextArg;
import static com.amazon.fusion.FusionTimestamp.isTimestamp;
import static com.amazon.fusion.FusionTimestamp.unsafeTimestampAnnotate;
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
            String a = checkNonEmptyTextArg(eval, this, i+1, args);
            annotations[i] = a;
        }

        Object target = args[0];
        if (isCollection(eval, target))
        {
            return unsafeCollectionAnnotate(eval, target, annotations);
        }

        if (isString(eval, target))
        {
            return unsafeStringAnnotate(eval, target, annotations);
        }

        if (isSymbol(eval, target))
        {
            return unsafeSymbolAnnotate(eval, target, annotations);
        }

        if (isBool(eval, target))
        {
            return unsafeBoolAnnotate(eval, target, annotations);
        }

        if (isInt(eval, target))
        {
            return unsafeIntAnnotate(eval, target, annotations);
        }

        if (isDecimal(eval, target))
        {
            return unsafeDecimalAnnotate(eval, target, annotations);
        }

        if (isTimestamp(eval, target))
        {
            return unsafeTimestampAnnotate(eval, target, annotations);
        }

        if (isNullNull(eval, target))
        {
            return makeNullNull(eval, annotations);
        }

        if (isFloat(eval, target))
        {
            return unsafeFloatAnnotate(eval, target, annotations);
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
