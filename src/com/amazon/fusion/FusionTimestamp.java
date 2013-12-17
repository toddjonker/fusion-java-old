// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionString.checkNullableStringArg;
import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import static com.amazon.ion.util.IonTextUtils.isDigit;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.math.BigDecimal;


final class FusionTimestamp
{
    private FusionTimestamp() {}



    //========================================================================
    // Representation


    abstract static class BaseTimestamp
        extends BaseValue
    {
        private BaseTimestamp() {}

        abstract Timestamp timestampValue();

        @Override
        BaseTimestamp annotate(Evaluator eval, String[] annotations)
        {
            return FusionTimestamp.annotate(this, annotations);
        }

        @Override
        SyntaxValue toStrippedSyntaxMaybe(Evaluator eval)
        {
            return makeSyntax(eval, /*location*/ null, this);
        }
    }


    private static class NullTimestamp
        extends BaseTimestamp
    {
        private NullTimestamp() {}

        @Override
        Timestamp timestampValue()
        {
            return null;
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseTimestamp
                         && ((BaseTimestamp) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullTimestamp();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.TIMESTAMP);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.timestamp");
        }
    }


    private static class ActualTimestamp
        extends BaseTimestamp
    {
        private final Timestamp myContent;

        private ActualTimestamp(Timestamp content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        Timestamp timestampValue()
        {
            return myContent;
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseTimestamp)
            {
                Timestamp rt = ((BaseTimestamp) right).timestampValue();
                if (myContent.equals(rt))
                {
                    return trueBool(eval);
                }
            }

            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseTimestamp)
            {
                Timestamp rt = ((BaseTimestamp) right).timestampValue();
                if (rt != null && myContent.compareTo(rt) == 0)
                {
                    return trueBool(eval);
                }
            }

            return falseBool(eval);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newTimestamp(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeTimestamp(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            myContent.print(out);
        }

        @Override
        void display(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            myContent.print(out);
        }
    }


    private static class AnnotatedTimestamp
        extends BaseTimestamp
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseTimestamp  myValue;

        private AnnotatedTimestamp(String[] annotations, BaseTimestamp value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        BaseTimestamp annotate(Evaluator eval, String[] annotations)
        {
            return FusionTimestamp.annotate(myValue, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        Timestamp timestampValue()
        {
            return myValue.timestampValue();
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.strictEquals(eval, right);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.tightEquals(eval, right);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = myValue.copyToIonValue(factory,
                                                 throwOnConversionFailure);
            iv.setTypeAnnotations(myAnnotations);
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(myAnnotations);
            myValue.ionize(eval, out);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            myValue.write(eval, out);
        }
    }



    //========================================================================
    // Constructors


    private static final BaseTimestamp NULL_TIMESTAMP = new NullTimestamp();


    /**
     * @param value may be null to make {@code null.timestamp}.
     *
     * @return not null.
     */
    static BaseTimestamp makeTimestamp(Evaluator eval, Timestamp value)
    {
        return (value == null ? NULL_TIMESTAMP : new ActualTimestamp(value));
    }


    private static BaseTimestamp annotate(BaseTimestamp unannotated,
                                          String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedTimestamp);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedTimestamp(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.timestamp}.
     *
     * @return not null.
     */
    static BaseTimestamp makeTimestamp(Evaluator eval,
                                       String[]  annotations,
                                       Timestamp value)
    {
        BaseTimestamp base = makeTimestamp(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseTimestamp unsafeTimestampAnnotate(Evaluator eval,
                                                 Object    fusionTimestamp,
                                                 String[]  annotations)
    {
        BaseTimestamp base = (BaseTimestamp) fusionTimestamp;
        return base.annotate(eval, annotations);
    }


    //========================================================================
    // Predicates


    public static boolean isTimestamp(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseTimestamp);
    }

    static boolean isTimestamp(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseTimestamp);
    }


    //========================================================================
    // Conversions


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     *
     * @return null if given {@code null.timestamp}.
     */
    static Timestamp unsafeTimestampToJavaTimestamp(Evaluator eval,
                                                    Object fusionTimestamp)
        throws FusionException
    {
        return ((BaseTimestamp) fusionTimestamp).timestampValue();
    }


    /**
     * Converts a Fusion timestamp to a {@link Timestamp}.
     *
     * @return null if the value isn't a Fusion timestamp.
     */
    static Timestamp timestampToJavaTimestamp(Evaluator eval, Object value)
        throws FusionException
    {
        if (isTimestamp(eval, value))
        {
            return unsafeTimestampToJavaTimestamp(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static Timestamp checkTimestampArg(Evaluator eval,
                                       Procedure who,
                                       String    expectation,
                                       int       argNum,
                                       Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseTimestamp)
        {
            return ((BaseTimestamp) arg).timestampValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    static Timestamp checkNullableTimestampArg(Evaluator eval,
                                               Procedure who,
                                               int argNum,
                                               Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable timestamp";
        return checkTimestampArg(eval, who, expectation, argNum, args);
    }


    @Deprecated
    static Timestamp checkNullableArg(Procedure who, int argNum,
                                      Object... args)
        throws FusionException, ArgTypeFailure
    {
        return checkNullableTimestampArg(null, who, argNum, args);
    }


    /**
     * @return not null
     */
    static Timestamp checkRequiredTimestampArg(Evaluator eval,
                                               Procedure who,
                                               int       argNum,
                                               Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null timestamp";
        Timestamp result =
            checkTimestampArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }



    //========================================================================
    // Procedures


    static final class IsTimestampProc
        extends Procedure1
    {
        IsTimestampProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `timestamp`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isTimestamp(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class StringToTimestampProc
        extends Procedure
    {
        StringToTimestampProc()
        {
            //    "                                                                               |
            super("Converts a `string` to a timestamp, recognizing (only) Ion formatted data.\n" +
                  "Returns `null.timestamp` when given `null.string`.\n" +
                  "\n" +
                  "    (string_to_timestamp null.string)        ==> null.timestamp\n" +
                  "    (string_to_timestamp \"2013-11-13T\")      ==> 2013-11-13\n" +
                  "    (string_to_timestamp \"null.timestamp\")   ==> ERROR",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableStringArg(eval, this, 0, args);

            Timestamp result;
            if (input == null)
            {
                result = null;
            }
            else
            {
                try
                {
                    result = Timestamp.valueOf(input);

                    // Don't accept input "null.timestamp". That's shady.
                    if (result == null)
                    {
                        throw argFailure("timestamp-formatted string", 0, args);
                    }

                    // Hack around Timestamp.valueOf() accepting stoppers.
                    char last = input.charAt(input.length() - 1);
                    if (! (last == 'Z' || last == 'T' || isDigit(last, 10)))
                    {
                        throw argFailure("timestamp-formatted string", 0, args);
                    }
                }
                catch (IllegalArgumentException e)
                {
                    throw argFailure("timestamp-formatted string", 0, args);
                }
            }

            return makeTimestamp(eval, result);
        }
    }


    static final class TimestampToStringProc
        extends Procedure
    {
        TimestampToStringProc()
        {
            //    "                                                                               |
            super("Converts a `timestamp` to a string in Ion format.\n" +
                  "Returns `null.string` when given `null.timestamp`.\n" +
                  "\n" +
                  "    (timestamp_to_string null.timestamp)   ==> null.string\n" +
                  "    (timestamp_to_string 2013-11-13T)      ==> \"2013-11-13\"",
                  "timestamp");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Timestamp input = checkNullableTimestampArg(eval, this, 0, args);

            String result = (input == null ? null : input.toString());
            return makeString(eval, result);
        }
    }


    static final class TimestampNowProc
        extends Procedure0
    {
        TimestampNowProc()
        {
            //    "                                                                               |
            super("Returns a timestamp representing \"now\".  At present the local offset is\n" +
                  "unspecified, but that may change in the future.");
        }

        @Override
        Object doApply(Evaluator eval)
            throws FusionException
        {
            Timestamp now = Timestamp.now();
            return makeTimestamp(eval, now);
        }
    }


    static final class TimestampToEpochMillisProc
        extends Procedure1
    {
        TimestampToEpochMillisProc()
        {
            //    "                                                                               |
            super("Given a non-null timestamp, returns the same point in time represented as the\n" +
                  "number of milliseconds since 1970-01-01T00:00Z.  The result is a decimal.",
                  "timestamp");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            Timestamp ts = checkRequiredTimestampArg(eval, this, 0, arg);
            BigDecimal millis = ts.getDecimalMillis();
            return makeDecimal(eval, millis);
        }
    }
}
