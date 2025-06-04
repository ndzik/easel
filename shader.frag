#version 410 core

in vec2 texCoord;
uniform float dt;
out vec3 color;

// Sphere data
vec3 sphereCenter(float t) {
    float angle = t; // rotation angle
    float radius = 2.0;
    return vec3(sin(angle) * radius, 0.0, cos(angle) * radius);
}

bool intersectSphere(vec3 ro, vec3 rd, vec3 center, float radius, out float t) {
    vec3 oc = ro - center;
    float b = dot(oc, rd);
    float c = dot(oc, oc) - radius * radius;
    float h = b * b - c;
    if (h < 0.0) return false;
    h = sqrt(h);
    t = -b - h;
    return t > 0.0;
}

vec3 computeNormal(vec3 p, vec3 center) {
    return normalize(p - center);
}

void main() {
    // Convert texCoord to [-1, 1]
    vec2 uv = texCoord * 2.0 - 1.0;

    // Camera setup
    vec3 ro = vec3(0.0, 0.0, 5.0);  // Ray origin
    vec3 rd = normalize(vec3(uv, -1.0)); // Ray direction

    // Rotate ray origin around Y axis
    float angle = dt;
    mat3 rotY = mat3(
        cos(angle), 0.0, -sin(angle),
        0.0,        1.0,  0.0,
        sin(angle), 0.0,  cos(angle)
    );
    ro = rotY * ro;
    rd = rotY * rd;

    // Sphere setup
    vec3 center = vec3(0.0, 0.0, 0.0);
    float radius = 1.0;

    float t;
    if (intersectSphere(ro, rd, center, radius, t)) {
        vec3 hit = ro + rd * t;
        vec3 normal = computeNormal(hit, center);
        vec3 lightDir = normalize(vec3(1.0, 1.0, 1.0));
        float diff = max(dot(normal, lightDir), 0.0);
        color = mix(vec3(0.1), vec3(1.0, 0.4, 0.2), diff);
    } else {
        // Background
        color = vec3(0.0, 0.1, 0.2) + 0.3 * uv.y;
    }
}
