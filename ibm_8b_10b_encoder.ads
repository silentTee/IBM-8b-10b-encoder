--Copyright (c) 2019 Alex Tsantilis under the MIT License

package IBM_8b_10b_Encoder is

    type Byte is mod 2**8;
    type Ten_Bits is mod 2**10;
    type Running_Disp is (Neg_1, Pos_1);

    procedure Encode(   B: in Byte; 
                        T: out Ten_Bits; 
                        RD: in out Running_Disp);

    --To verify that Running Disparity is being preserved, use this version
    procedure Decode(   T: in Ten_Bits; 
                        B: out Byte; 
                        RD: in out Running_Disp; 
                        Success: out Boolean);

    procedure Decode(   T: in Ten_Bits; 
                        B: out Byte; 
                        Success: out Boolean);
                        
end IBM_8b_10b_Encoder;
