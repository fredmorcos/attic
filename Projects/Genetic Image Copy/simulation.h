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

#ifndef SIMULATION_H
#define SIMULATION_H

#include <QObject>
#include <QImage>

class Simulation : public QObject
{
	Q_OBJECT

private:
	QImage m_originalImage, m_copyImage;
	bool m_shouldStop, m_mutationHappened;

	QImage generateRandomIndividual();
	QImage mutateIndividual(QImage);
	uint randomColor();
	uint mutateColor(uint);
	double pixelFitness(int, int);
	double individualFitness(QImage);

public:
	Simulation(QObject *);

public slots:
	void start(QImage);
	void stop();

signals:
	void imageUpdated(const QImage);
};

#endif // SIMULATION_H
