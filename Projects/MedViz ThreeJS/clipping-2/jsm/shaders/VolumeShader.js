import {
    Vector2,
    Vector3
} from '../../build/three.module.js';

/**
 * Shaders to render 3D volumes using raycasting.
 * The applied techniques are based on similar implementations in the Visvis and Vispy projects.
 * This is not the only approach, therefore it's marked 1.
 */

const VolumeRenderShader1 = {
    uniforms: {
	'u_size': {
            value: new Vector3(1, 1, 1)
        },
	'u_renderstyle': {
            value: 0
        },
	'u_renderthreshold': {
            value: 0.5
        },
	'u_clim': {
            value: new Vector2(1, 1)
        },
	'u_data': {
            value: null
        },
	'u_cmdata': {
            value: null
        }
    },
    vertexShader: '',
    fragmentShader: ''
};

export { VolumeRenderShader1 };
