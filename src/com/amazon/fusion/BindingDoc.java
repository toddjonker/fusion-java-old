// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class BindingDoc
{
    enum Kind { PROCEDURE, SYNTAX, CONSTANT }

    final String myName;
    final Kind   myKind;
    // TODO one-liner
    // TODO intro
    // TODO pairs of usage/body
    final String myUsage;
    final String myBody;

    BindingDoc(String name, Kind kind, String usage, String body)
    {
        myName = name;
        myKind = kind;
        myUsage = usage;
        myBody = body;
    }
}
