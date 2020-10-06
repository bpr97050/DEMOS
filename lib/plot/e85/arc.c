/*
 * Рисование дуги из точки x0,y0 в x1,y1 против час. стрелки
 * с центром в x,y  на Э-85
 * by @VG
 */

int _Ox, _Oy;

arc(x, y, x0, y0, x1, y1)
{
	int xx = _Ox, yy = _Oy, c;
	double sqrt();
	extern double scalex;
	long r;

	/*
	 * Вычислим радиус
	 */
	r = scalex*sqrt( (x-x0)*(double)(x-x0) + (y-y0)*(double)(y-y0) );

	/*
	 * Выбор дуги окружности
	 * Если элемент z векторного произведения векторов
	 * (x,y)->(x0,y0) и (x,y)->(x1,y1) имеет положительный/отрицательный
	 * знак, то рисовать по/против часовой стрелки
	 */
	if( (x0-x)*(y1-y) < (x1-x)*(y0-y) )
		r = -r;

	/*
	 * Установим радиус, начальную точку и
	 * нарисуем до конечной точки
	 */
	putch(034);
	putch( ((short)(r>>12) & 077) + '@' );
	putch( ((short)(r>> 6) & 077) + '@' );
	putch( ((short) r      & 077) + '@' );
	_DotC(036, x0, y0);
	_DotC(032, x1, y1);

	_DotC(036, xx, yy);
}