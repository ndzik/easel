#version 410 core

in vec2 texCoord;
uniform float dt;
out vec3 color;

#define MAX_STEPS 128
#define MAX_DIST 100.0
#define SURF_DIST 0.001

// Signed Distance Function for a centered box
float sdBox(vec3 p, vec3 b) {
    vec3 q = abs(p) - b;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
}

// Scene SDF: single cube
float map(vec3 p) {
    return sdBox(p, vec3(1.0));
}

// Normal estimation
vec3 estimateNormal(vec3 p) {
    float eps = 0.0005;
    return normalize(vec3(
        map(p + vec3(eps, 0.0, 0.0)) - map(p - vec3(eps, 0.0, 0.0)),
        map(p + vec3(0.0, eps, 0.0)) - map(p - vec3(0.0, eps, 0.0)),
        map(p + vec3(0.0, 0.0, eps)) - map(p - vec3(0.0, 0.0, eps))
    ));
}

// Raymarching loop
float rayMarch(vec3 ro, vec3 rd, out vec3 pHit) {
    float dist = 0.0;
    for (int i = 0; i < MAX_STEPS; ++i) {
        vec3 p = ro + rd * dist;
        float d = map(p);
        if (d < SURF_DIST) {
            pHit = p;
            return dist;
        }
        dist += d;
        if (dist >= MAX_DIST) break;
    }
    return -1.0;
}

void main() {
    vec2 uv = texCoord * 2.0 - 1.0;
    uv.x *= 16.0/9.0; // aspect correction

    // Camera rotation around Y
    float angle = dt * 0.5;
    mat3 rotY = mat3(
        cos(angle), 0.0, -sin(angle),
        0.0,        1.0,  0.0,
        sin(angle), 0.0,  cos(angle)
    );

    vec3 ro = rotY * vec3(0.0, 0.0, 5.0);
    vec3 lookAt = vec3(0.0);
    vec3 forward = normalize(lookAt - ro);
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);

    vec3 rd = normalize(uv.x * right + uv.y * up + 1.5 * forward);

    vec3 pHit;
    float dist = rayMarch(ro, rd, pHit);
    if (dist > 0.0) {
        vec3 n = estimateNormal(pHit);
        vec3 lightDir = normalize(vec3(1.0, 2.0, 1.0));
        float diff = max(dot(n, lightDir), 0.0);
        color = mix(vec3(0.1), vec3(0.3, 0.6, 1.0), diff);
    } else {
        color = vec3(0.02, 0.02, 0.05); // background
    }
}
