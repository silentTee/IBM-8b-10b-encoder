--Copyright (c) 2019 Alex Tsantilis under the MIT License

package body IBM_8b_10b_Encoder is    
    type Encoder_Entry is
    record
        RD_Neg_Val: Ten_Bits;
        RD_Pos_Val: Ten_Bits;
        RD_Changes: Boolean;
    end record;
    
    type Decoder_Entry is
    record
        Key: Ten_Bits := 0;
        Value: Byte := 0;
    end record;
    
    type Decoder_Table is array(Natural range 0 .. 255) of Decoder_Entry;
    
    Encoder_Table: array(Byte) of Encoder_Entry;
    Decoder_Table_RD_Neg: Decoder_Table;
    Decoder_Table_RD_Pos: Decoder_Table;
    
    procedure Encode(   B: in Byte; 
                        T: out Ten_Bits; 
                        RD: in out Running_Disp) is
    begin
        case RD is
            when Neg_1 =>   T := Encoder_Table(B).RD_Neg_Val;
                            case Encoder_Table(B).RD_Changes is
                                when False => RD := Neg_1;
                                when True  => RD := Pos_1;
                            end case;
            when Pos_1 =>   T := Encoder_Table(B).RD_Pos_Val;
                            case Encoder_Table(B).RD_Changes is
                                when False => RD := Pos_1;
                                when True  => RD := Neg_1;
                            end case;
        end case;
    end Encode;
    
    function Compute_Hash_Index(K: Ten_Bits;
                                Table_Size: Natural) return Natural is
    begin
        return Natural(K) mod Table_Size;
    end Compute_Hash_Index;
    
    procedure Insert(   Table: in out Decoder_Table; 
                        Key: in Ten_Bits; 
                        Value: in Byte;
                        Success: out Boolean ) is
        I: Natural := 0;
        Idx, Idx_Modded: Natural;
    begin
        Success := False;
        Idx := Compute_Hash_Index(Key, Table'Length);
        Idx_Modded := Idx;
        loop
            if Table(Idx_Modded).Key = 0 or Table(Idx_Modded).Key = Key then
                Table(Idx_Modded).Key := Key;
                Table(Idx_Modded).Value := Value;
                Success := True;
            else
                I := I + 1;
                --Note: this variant of the quadratic probing algorithm
                --is most effective with tables with 2^n entries, where
                --n is any positive integer. Coincidentally, this works
                --perfectly for our encoding table. :D
                Idx_Modded := (Idx + (I + I**2)/2) mod Table'Length;
            end if;
            exit when Success or I >= Table'Length;
        end loop;
    end Insert;
    
    procedure Find_Entry(   Table: in Decoder_Table; 
                            Key: in Ten_Bits; 
                            Value: out Byte;
                            Success: out Boolean) is
        I: Natural := 0;
        Idx, Idx_Modded: Natural;
    begin
        Success := False;
        Idx := Compute_Hash_Index(Key, Table'Length);
        Idx_Modded := Idx;
        loop
            if Table(Idx_Modded).Key = Key then
                Value := Table(Idx_Modded).Value;
                Success := True;
            else
                I := I + 1;
                Idx_Modded := (Idx + (I + I**2)/2) mod Table'Length;
            end if;
            exit when Success or I >= Table'Length;
        end loop;
    end Find_Entry;

    --To verify that Running Disparity is being preserved, use this version.
    procedure Decode(   T: in Ten_Bits; 
                        B: out Byte; 
                        RD: in out Running_Disp; 
                        Success: out Boolean) is
    begin
        case RD is
            when Neg_1 =>
                Find_Entry(Decoder_Table_RD_Neg, T, B, Success);
                case Encoder_Table(B).RD_Changes is
                    when False => RD := Neg_1;
                    when True  => RD := Pos_1;
                end case;
            when Pos_1 =>
                Find_Entry(Decoder_Table_RD_Pos, T, B, Success);
                case Encoder_Table(B).RD_Changes is
                    when False => RD := Pos_1;
                    when True  => RD := Neg_1;
                end case;
        end case;
    end Decode;

    procedure Decode(   T: in Ten_Bits; 
                        B: out Byte; 
                        Success: out Boolean) is
    begin
        Find_Entry(Decoder_Table_RD_Neg, T, B, Success);
        if not Success then
            Find_Entry(Decoder_Table_RD_Pos, T, B, Success);
        end if;
    end Decode;
begin
    declare
        --8 to 10 bit encoding table construction necessities (all are temporary)
        Table_5to6_Bits: array(Byte range 0 .. 31, 0 .. 1) of Ten_Bits :=
        ((2#111001#, 2#000110#),
         (2#101110#, 2#010001#),
         (2#101101#, 2#010010#),
         (2#100011#, 2#100011#),
         (2#101011#, 2#010100#),
         (2#100101#, 2#100101#),
         (2#100110#, 2#100110#),
         (2#000111#, 2#111000#),
         (2#100111#, 2#011000#),
         (2#101001#, 2#101001#),
         (2#101010#, 2#101010#),
         (2#001011#, 2#001011#),
         (2#101100#, 2#101100#),
         (2#001101#, 2#001101#),
         (2#001110#, 2#001110#),
         (2#111010#, 2#000101#),
         (2#110110#, 2#001001#),
         (2#110001#, 2#110001#),
         (2#110010#, 2#110010#),
         (2#010011#, 2#010011#),
         (2#110100#, 2#110100#),
         (2#010101#, 2#010101#),
         (2#010110#, 2#010110#),
         (2#010111#, 2#101000#),
         (2#110011#, 2#001100#),
         (2#011001#, 2#011001#),
         (2#011010#, 2#011010#),
         (2#011011#, 2#100100#),
         (2#011100#, 2#011100#),
         (2#011101#, 2#100010#),
         (2#011110#, 2#100001#),
         (2#110101#, 2#001010#));
        
        Table_3to4_Bits: array(Byte range 0 .. 7, 0 .. 3) of Ten_Bits :=
        --RD -1, RD +1
        ((2#1101#, 2#0010#, -1, -1),
         (2#1001#, 2#1001#, -1, -1),
         (2#1010#, 2#1010#, -1, -1),
         (2#0011#, 2#1100#, -1, -1),
         (2#1011#, 2#0100#, -1, -1),
         (2#0101#, 2#0101#, -1, -1),
         (2#0110#, 2#0110#, -1, -1),

        --||---Primary----|||---Alternate---||
        --RD -1   , RD +1  | RD -1  , RD +1
         (2#0111#, 2#1000#, 2#1110#, 2#0001#));    
        
            
        NO  : Boolean := False;
        YES : Boolean := True;
        Changes_Running_Disp: array(Byte range 0 .. 31) of Boolean :=
        (
            NO ,
            NO ,
            NO ,
            YES,
            NO ,
            YES,
            YES,
            YES,
            NO ,
            YES,
            YES,
            YES,
            YES,
            YES,
            YES,
            NO ,
            NO ,
            YES,
            YES,
            YES,
            YES,
            YES,
            YES,
            NO ,
            NO ,
            YES,
            YES,
            NO ,
            YES,
            NO ,
            NO ,
            NO
        );
        
        Old_Right, Old_Left: Byte := 0;
        Encoded_Left_Pos, Encoded_Left_Neg, 
        Encoded_Right_Pos, Encoded_Right_Neg: Ten_Bits := 0;
        RD_Change_Idx: Byte;
        
        Insertion_Error: exception;
    begin
        for I in Byte range 0 .. 255 loop
            Old_Right := I and 2#000_11111#;
            Old_Left := (I and 2#111_00000#) / 2**5;
            
            --Encoding the right 5 bits is easy, but encoding the left
            --3 bits requires following a very specific pattern...
            Encoded_Right_Neg := Table_5to6_Bits(Old_Right, 0);
            Encoded_Right_Pos := Table_5to6_Bits(Old_Right, 1);
            
            if Old_Left < 2#111# then
                --when the encoding of the right bits is the same regardless
                --of the running disparity (7 is an exception because it has 
                --equal 0s and 1s), the 5 bit encoding for (-) RD is paired with 
                --the 3 bit encoding for (-) RD, and (+) with (+)
                if Encoded_Right_Neg = Encoded_Right_Pos or Old_Right = 7 then
                    Encoded_Left_Neg := Table_3to4_Bits(Old_Left, 0);
                    Encoded_Left_Pos := Table_3to4_Bits(Old_Left, 1);
                else
                    Encoded_Left_Neg := Table_3to4_Bits(Old_Left, 1);
                    Encoded_Left_Pos := Table_3to4_Bits(Old_Left, 0);
                end if;
            
            --when the 3 left bits to encode are "111", there are unique
            --circumstances where an alternate output needs to be used
            else
                if Encoded_Right_Neg = Encoded_Right_Pos or Old_Right = 7 then
                    if Old_Right /= 17 and Old_Right /= 18 and Old_Right /= 20 then
                        Encoded_Left_Neg := Table_3to4_Bits(Old_Left, 0);
                    else
                        Encoded_Left_Neg := Table_3to4_Bits(Old_Left, 2);
                    end if;
                    
                    if Old_Right /= 11 and Old_Right /= 13 and Old_Right /= 14 then
                        Encoded_Left_Pos := Table_3to4_Bits(Old_Left, 1);
                    else
                        Encoded_Left_Pos := Table_3to4_Bits(Old_Left, 3);
                    end if;
                else
                    Encoded_Left_Neg := Table_3to4_Bits(Old_Left, 1);
                    Encoded_Left_Pos := Table_3to4_Bits(Old_Left, 0);
                end if;
            end if;
            
            RD_Change_Idx := I mod 32;
            
            declare
                E: Encoder_Entry;
                Check: Boolean;
            begin
                --Add an entry to the encoding table...
                E.RD_Neg_Val := (Encoded_Left_Neg * 2**6) or Encoded_Right_Neg;
                E.RD_Pos_Val := (Encoded_Left_Pos * 2**6) or Encoded_Right_Pos;
                if Old_Left = 2#000# or Old_Left = 2#100# or Old_Left = 2#111# then
                    E.RD_Changes := Changes_Running_Disp(RD_Change_Idx);
                else
                    E.RD_Changes := not Changes_Running_Disp(RD_Change_Idx);
                end if;
                Encoder_Table(I) := E;
                
                --...and add the flipped keys and values to the decoding tables
                Insert(Decoder_Table_RD_Neg, E.RD_Neg_Val, I, Check);
                Insert(Decoder_Table_RD_Pos, E.RD_Pos_Val, I, Check);
                
                if not Check then raise Insertion_Error; end if;
            end;
        end loop;
    end;
    
end IBM_8b_10b_Encoder;
