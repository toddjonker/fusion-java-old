// Copyright (c) 2023 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.dispatchWrite;
import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionSymbol.checkRequiredSymbolArg;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import java.io.IOException;

/**
 * Implementation of records; based on Racket's "structs".
 */
final class Records
{
    private final static class RecordType
        extends BaseValue
    {
        private final BaseSymbol myName;
        private final int        myFieldCount;

        public RecordType(BaseSymbol name, int fieldCount)
        {
            myName = name;
            myFieldCount = fieldCount;
        }

        RecordInstance cast(Object v)
        {
            if (v instanceof RecordInstance)
            {
                RecordInstance i = (RecordInstance) v;
                if (i.myType == this) return i;
            }
            return null;
        }

        void writeName(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            myName.write(eval, out);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("{{{record_type ");
            writeName(eval, out);
            out.append("}}}");
        }
    }


    private final static class RecordInstance
        extends BaseValue
    {
        private final RecordType myType;
        private final Object[] myFields;

        RecordInstance(RecordType type, Object[] fields)
        {
            myType   = type;
            myFields = fields;
        }

        Object unsafe_access(int i)
        {
            return myFields[i];
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO Support opaque records
            myType.writeName(eval, out);
            out.append("::{[");

            int length = myFields.length;
            for (int i = 0; i < length; i++)
            {
                if (i != 0) out.append(", ");
                dispatchWrite(eval, out, myFields[i]);
            }

            out.append("]}");
        }
    }


    private final static class RecordConstructorProc
        extends Procedure
    {
        private final RecordType myType;
        private final int        myInitFieldCount;

        public RecordConstructorProc(RecordType type, int initFieldCount)
        {
            myType           = type;
            myInitFieldCount = initFieldCount;
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(myInitFieldCount, args);
            // TODO apply guardian
            return new RecordInstance(myType, args);

        }
    }

    private final static class RecordPredicateProc
        extends Procedure1
    {
        private final RecordType myType;

        public RecordPredicateProc(RecordType type)
        {
            myType = type;
        }

        @Override
        Object doApply(Evaluator eval, Object v)
        {
            RecordInstance record = myType.cast(v);
            return FusionBool.makeBool(eval, record != null);
        }
    }


    private final static class RecordAccessorProc
        extends Procedure2
    {
        private final RecordType myType;

        public RecordAccessorProc(RecordType type)
        {
            myType = type;
        }

        @Override
        Object doApply(Evaluator eval, Object v, Object index)
            throws FusionException
        {
            RecordInstance record = myType.cast(v);
            if (record == null)
            {
                throw new ArgumentException(this, "instance of " + myType.myName,
                                            0, v, index);
            }

            int i = checkIntArgToJavaInt(eval, this, 1, v, index);
            if (i < 0)
            {
                throw new ArgumentException(this, "non-negative int",
                                            1, v, index);
            }

            int max = myType.myFieldCount - 1;
            if (max < 0)
            {
                throw new ArgumentException(this, "no access to fields",
                                            -1, v, index);
            }
            if (max < i)
            {
                throw new ArgumentException(this, "index up to " + max,
                                            1, v, index);
            }

            return record.unsafe_access(i);
        }
    }


    /*
     * Currently:
     *   (make_record_type name_sym field_count)
     *
     * Eventually this should conform to Racket's protocol:
     * (make-struct-type name super-type
     *   init-field-cnt auto-field-cnt
     *   [ auto-v props inspector proc-spec immutables guard constructor-name])
     */
    final static class MakeRecordTypeProc
        extends Procedure
    {
        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(2, args);

            String name = checkRequiredSymbolArg(eval, this, 0, args);
            if (name.length() == 0)
            {
                throw new ArgumentException(this, "non-empty symbol", 0, args);
            }

            final int initFieldCount = checkIntArgToJavaInt(eval, this, 1, args);
            if (initFieldCount < 0)
            {
                throw new ArgumentException(this, "non-negative int", 1, args);
            }

            RecordType type
                = new RecordType((BaseSymbol) args[0], initFieldCount);
            Procedure ctor
                = new RecordConstructorProc(type, initFieldCount);
            Procedure pred
                = new RecordPredicateProc(type);
            Procedure accessor
                = new RecordAccessorProc(type);

            // TODO verify correct inferred object names
            ctor.inferName(name);
            pred.inferName("is_" + name);
            accessor.inferName(name + "_get");

            return new Object[] { type, ctor, pred, accessor };
        }
    }
}
