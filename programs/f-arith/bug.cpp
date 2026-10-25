#include <cstdint>
#include <vector>
#include <iostream>

class integer {
private:
    bool _sign;
    std::vector<uint32_t> _digits;

public:

    integer () : _sign(false), _digits{} {}

    integer (std::vector<uint8_t> const &bytes)
    : _sign(false), _digits{}
    {
        // first we process the sequence of 7-bit digits
	std::vector<uint8_t> digits;
	uint8_t b = bytes[0];
	this->_sign = ((b & 0x40) != 0);
	digits.push_back(b & 0x3f);
        for (int i = 1;  i < bytes.size();  ++i) {
	    digits.push_back(bytes[i] & 0x7f);
	};

	b = digits[0];

        // the number of significant bits in the first byte
	int firstBits;
	if (b > 0x1f) firstBits = 6;
	else if (b > 0xf) firstBits = 5;
	else if (b > 0x7) firstBits = 4;
	else if (b > 0x3) firstBits = 3;
	else if (b > 0x1) firstBits = 2;
	else if (b > 0x0) firstBits = 1;
	else firstBits = 0;

        // the total number of bits (not counting the sign) is computed
        // as 7 bits per continuation byte plus the bits required for the
        // first byte
	int nbits = (digits.size() - 1) * 7 + firstBits;

	if (nbits > 0) {
	    this->_digits.reserve((nbits + 31) >> 5);
	    // the number of bits available in the current digit
	    uint32_t availBits = nbits % 32;
	    if (availBits == 0) { availBits = 32; }
	    // initialize the current result digit to the first byte
            uint32_t w = b;
            if (availBits < firstBits) {
                int k = firstBits - availBits;
                this->_digits.push_back(w >> k);
                w &= (1 << k) - 1;
                availBits = 32 - k;
            } else {
                availBits -= firstBits;
            }
	    int idx = 1;
	    while (idx < digits.size()) {
		b = digits[idx];
		if (availBits >= 7) {
		    w = (w << 7) | b;
		    availBits -= 7;
		} else {
		    uint32_t excessBits = 7 - availBits;
		  // first we extract the high availBits from b
		    w = (w << availBits) | (b >> excessBits);
		    this->_digits.push_back(w);
		    availBits = 32 - excessBits;
		    w = b & ((1 << excessBits) - 1);
		}
		idx++;
	    }
	    this->_digits.push_back(w);
	  // here availBits == 0
	}
	// else zero

    }

    bool toInt64 (int64_t &n) const
    {
	if (this->_digits.size() == 0) {
	    n = 0;
	    return false;
	}
	else if (this->_digits.size() == 1) {
	    n = this->_sign
		? - static_cast<int64_t>(this->_digits[0])
		: static_cast<int64_t>(this->_digits[0]);
	    return false;
	}
	else if (this->_digits.size() == 2) {
	  // negative number
	    uint64_t un = (static_cast<uint64_t>(this->_digits[0]) << 32)
		+ static_cast<uint64_t>(this->_digits[1]);
	    if (this->_sign) {
	      // negative number
		if (un <= 0x8000000000000000) {
		    n = - static_cast<int64_t>(un);
		    return false;
		}
	    }
	    else if (un <= 0x7fffffffffffffff) {
		n = static_cast<int64_t>(un);
		return false;
	    }
	}

	return true;

    } // toInt64

    std::string to_string ()
    {
        if (this->_digits.empty()) {
            return "0";
        }
        std::string s = (this->_sign ? "-" : "+") + std::to_string(this->_digits[0]);
        for (int i = 1;  i < this->_digits.size();  ++i) {
            s += "_" + std::to_string(this->_digits[i]);
        }
        return s;
    }
};

int main ()
{
    std::vector<uint8_t> n_bytes = { 0x92, 0xD0, 0x97, 0xE4, 0x0 };

    integer big_n = integer(n_bytes);

    std::cout << "bigN = " << big_n.to_string() << "\n";

    int64_t n;
    big_n.toInt64 (n);

    std::cout << "N = " << n << "\n";

}
