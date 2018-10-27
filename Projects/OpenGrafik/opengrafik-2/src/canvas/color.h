#ifndef COLOR_H_
#define COLOR_H_

class Color
{
	private:
		double R, G, B, A;
		inline void roundColorValue(double& x);

	public:
		Color
			(double r = 0.0, double g = 0.0, double b = 0.0, double a = 0.0);
			
		void setColor
			(double r = 0.0, double g = 0.0, double b = 0.0, double a = 0.0);

		void setColor(Color& c);
		
		void setR(double r = 0.0);
		void setG(double g = 0.0);
		void setB(double b = 0.0);
		void setA(double a = 0.0);
		double getR();
		double getG();
		double getB();
		double getA();
};

#endif /* COLOR_H_ */
