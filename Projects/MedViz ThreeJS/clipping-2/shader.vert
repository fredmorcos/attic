varying vec4 v_nearpos;
varying vec4 v_farpos;
varying vec3 v_position;

// #include <clipping_planes_pars_vertex>
// varying vec3 vClipPosition;

#if NUM_CLIPPING_PLANES > 0
uniform vec4 clippingPlanes[NUM_CLIPPING_PLANES];
varying float distanceFromClippingPlane[NUM_CLIPPING_PLANES];
#endif

void main() {

// #include <begin_vertex>
    // vec3 transformed = vec3( position );

    // Prepare transforms to map to "camera view". See also:
    // https://threejs.org/docs/#api/renderers/webgl/WebGLProgram
    // mat4 viewtransformf = modelViewMatrix;
    // mat4 viewtransformi = inverse(modelViewMatrix);
    mat4 modelViewMatrixInverse = inverse(modelViewMatrix);

    // Project local vertex coordinate to camera position. Then do a step
    // backward (in cam coords) to the near clipping plane, and project back. Do
    // the same for the far clipping plane. This gives us all the information we
    // need to calculate the ray and truncate it to the viewing cone.
    vec4 position4 = vec4(position, 1.0);
    vec4 pos_in_cam = modelViewMatrix * position4;

    // Intersection of ray and near clipping plane (z = -1 in clip coords)
    pos_in_cam.z = -pos_in_cam.w;
    v_nearpos = modelViewMatrixInverse * pos_in_cam;

    // Intersection of ray and far clipping plane (z = +1 in clip coords)
    pos_in_cam.z = pos_in_cam.w;
    v_farpos = modelViewMatrixInverse * pos_in_cam;

    // Set varyings and output pos
    v_position = position;

    // vec4 final_position = projectionMatrix * viewMatrix * modelMatrix * position4;
    vec4 modelPosition = modelMatrix * position4;
    gl_Position = projectionMatrix * viewMatrix * modelPosition;

#if NUM_CLIPPING_PLANES > 0
#pragma unroll_loop_start
    for (int i = 0; i < NUM_CLIPPING_PLANES; i++) {
        distanceFromClippingPlane[i] = dot(modelPosition, clippingPlanes[i]);
    }
#pragma unroll_loop_end
#endif

// #include <project_vertex>
    // vec4 mvPosition = modelViewMatrix * vec4( transformed, 1.0 );
    // gl_Position = projectionMatrix * mvPosition;

// #include <clipping_planes_vertex>
    // vClipPosition = -mvPosition.xyz;
}
