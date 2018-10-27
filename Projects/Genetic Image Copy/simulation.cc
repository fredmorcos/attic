/*
This file is part of Fred Morcos' Genetic.

Fred Morcos' Genetic is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Fred Morcos' Genetic is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Fred Morcos' Genetic.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "simulation.h"

#include <QTime>
#include <math.h>

#define FITNESS_THRESHOLD 5000
#define COLOR_MUTATION_RANGE 20

Simulation::Simulation(QObject *parent):
		QObject(parent),
		m_shouldStop(false),
		m_mutationHappened(true)
{
}

/*
 * main run, always accept the next mutation of the current 
 * individual since the mutation operator is random and we 
 * do not mutate good pixels anyways.
 */
void Simulation::start(QImage originalImage)
{
	int w, h;

	srandom(QTime::currentTime().msec());

	m_originalImage = originalImage;
	m_shouldStop = false;
	m_mutationHappened = true;

	m_copyImage = generateRandomIndividual();
	w = m_copyImage.width();
	h = m_copyImage.height();

	while (!m_shouldStop && 
			   individualFitness(m_copyImage) > 
				 ((w * h) * FITNESS_THRESHOLD / 2) &&
				 m_mutationHappened)
	{
		m_copyImage = mutateIndividual(m_copyImage);
		emit imageUpdated(m_copyImage);
	}
}

/*
 * get the fitness of a pixel which is the distance (diff)
 * between it and the corresponding pixel in the original
 * image. the less the distance (hence the less the fitness 
 * value), the better the pixel is.
 */
double Simulation::pixelFitness(int x, int y)
{
	uint originalPixel = m_originalImage.pixel(x, y),
		 copyPixel = m_copyImage.pixel(x, y);
	double diffRed = qRed(originalPixel) - qRed(copyPixel),
		   diffGreen = qGreen(originalPixel) - qGreen(copyPixel),
		   diffBlue = qBlue(originalPixel) - qBlue(copyPixel);

	return (diffRed * diffRed) +
		   (diffGreen * diffGreen) +
		   (diffBlue * diffBlue);
}

/*
 * get the fitness of an individual, which is the sum of 
 * fitnesses of all its pixels. this is a minimization 
 * problem, so the less the fitness value, the better the 
 * individual.
 */
double Simulation::individualFitness(QImage img)
{
	double fitness = 0;

	for (int i = 0; i < img.width(); i++)
		for (int j = 0; j < img.height(); j++)
			fitness += pixelFitness(i, j);

	return fitness;
}

void Simulation::stop()
{
	m_shouldStop = true;
}

/*
 * mutate a random number of pixels with fitnesses lower than a 
 * threshold in an individual. mutation is done by changing some 
 * pixels to random colors. remember that this is a minimization 
 * problem, hence the if (pixelFitness > threshold) then mutate.
 */
QImage Simulation::mutateIndividual(QImage img)
{
	QImage newImg = img;
	m_mutationHappened = false;
	int width = newImg.width(),
		height = newImg.height(),
		numOfPixels = (random() % (width * height)),
		x = 0, y = 0;

	for (int i = 0; i < numOfPixels; i++)
	{
		x = random() % width;
		y = random() % height;

		if (pixelFitness(x, y) > FITNESS_THRESHOLD)
		{
			newImg.setPixel(x, y, randomColor());
			m_mutationHappened = true;
		}
	}

	return newImg;
}

uint Simulation::mutateColor(uint col)
{
	return qRgb(qRed(col) + ((random() % (COLOR_MUTATION_RANGE * 2)) - COLOR_MUTATION_RANGE),
              qGreen(col) + ((random() % (COLOR_MUTATION_RANGE * 2)) - COLOR_MUTATION_RANGE),
              qBlue(col) + ((random() % (COLOR_MUTATION_RANGE * 2)) - COLOR_MUTATION_RANGE));
}

uint Simulation::randomColor()
{
	return qRgb(random() % 255, random() % 255, random() % 255);
}

/*
 * generate random colors for the pixels in a new individual. used 
 * to create the first individual in the population.
 */
QImage Simulation::generateRandomIndividual()
{
	QImage img(m_originalImage.size(), m_originalImage.format());

	for (int i = 0; i < img.width(); i++)
		for (int j = 0; j < img.height(); j++)
			img.setPixel(i, j, randomColor());

	return img;
}

